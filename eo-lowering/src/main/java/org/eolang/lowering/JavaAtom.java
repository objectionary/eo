/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.stream.Collectors;

/**
 * The Java body of one lowered fragment.
 *
 * <p>A protocol is a program of steps, so its Java is one statement per
 * step: one local per void any step reads, one local per application,
 * one blank final per fork, assigned at the end of each of its two
 * arms, whose own steps sit inside the arm's block and so compute only
 * when the arm is taken, and one return handing the answer to
 * {@code Data.ToPhi}; the {@link Rendering} spells every value. A void
 * is read at the top of the innermost block that reaches every use of
 * it: at the top of the body when a step outside every fork reads it, or
 * both arms of one fork do, and at the top of an arm when that arm alone
 * does, so that an argument a guard protects is never forced while the
 * guard holds it back. A program that repeats runs inside
 * {@code while (true)}: its voids are locals the loop rebinds rather than
 * finals, so every one of them is read before the loop, the answer of
 * the fork that ends the program is assigned and followed by
 * {@code break}, and a repeat assigns the voids their next values,
 * through temporaries wherever a value names a void the same repeat
 * rebinds, and continues. A
 * program of several bodies runs the same loop over a state naming the
 * body that runs next: the voids of every body are locals, the
 * formation's read before the loop and the helpers' blank until a repeat
 * hands them values, each body is one branch on the state, a body that
 * answers assigns the one answer and breaks, and a repeat assigns the
 * voids of the body it resumes, then the state, and continues. The
 * text is exactly what the {@code lambda()} of the generated atom class
 * holds, indented for that spot, and it is the content the sidecar file
 * is named after. A protocol the rendering refuses, or one whose answer
 * no {@code Data.ToPhi} argument names, is refused too, and the caller
 * treats the refusal as one fragment staying unlowered.</p>
 *
 * @since 0.76.0
 */
public final class JavaAtom {

    /**
     * The program to render.
     */
    private final Program program;

    /**
     * The spelling of the values.
     */
    private final Rendering values;

    /**
     * Ctor, for a program of one body.
     * @param proto The protocol to render
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public JavaAtom(final Protocol proto, final Map<String, String> inputs) {
        this(
            new Program(
                Collections.singletonList(
                    new Body("", 0, new ArrayList<>(inputs.values()), proto)
                ),
                inputs
            )
        );
    }

    /**
     * Ctor.
     * @param plan The program to render
     */
    public JavaAtom(final Program plan) {
        this.program = plan;
        this.values = new Rendering(plan);
    }

    /**
     * The body of the {@code lambda()} method.
     * @return Java statements, one per line, without a trailing newline
     */
    public String text() {
        if ("string".equals(this.program.carrier())) {
            throw new IllegalStateException(
                "A string answer cannot be handed over, since Data.ToPhi makes bytes of a byte array"
            );
        }
        final Protocol first = this.program.bodies().get(0).protocol();
        final List<String> lines;
        if (this.program.bodies().size() > 1) {
            lines = this.resumed();
        } else if (first.repeats()) {
            lines = this.looped();
            lines.add(
                String.format(
                    "return new Data.ToPhi(%s);", this.values.expression(first.answer())
                )
            );
        } else {
            lines = this.computed(first, "", Collections.emptySet(), "");
            lines.add(
                String.format(
                    "return new Data.ToPhi(%s);", this.values.expression(first.answer())
                )
            );
        }
        return lines.stream()
            .map(line -> String.format("        %s", line))
            .collect(Collectors.joining(System.lineSeparator()));
    }

    private List<String> looped() {
        final Protocol first = this.program.bodies().get(0).protocol();
        final String exit = first.answer();
        if (!exit.startsWith("sym:s") || this.values.step(exit.substring(4)).branches().isEmpty()
            || this.values.forma(exit).isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "A program that repeats must answer through a fork, but it answers '%s'",
                    exit
                )
            );
        }
        final Step last = this.values.step(exit.substring(4));
        final int inputs = this.program.inputs().size();
        final List<String> out = new ArrayList<>(inputs + 4);
        final Set<Integer> all = new HashSet<>(inputs);
        for (int idx = 0; idx < inputs; ++idx) {
            out.add(this.values.reading(idx));
            all.add(idx);
        }
        out.add(String.format("final %s %s;", this.values.type(exit), last.label()));
        out.add("while (true) {");
        out.addAll(this.computed(first, "    ", all, last.label()));
        out.add("}");
        return out;
    }

    private List<String> resumed() {
        final int inputs = this.program.inputs().size();
        final int total = this.program.formas().size();
        final List<String> out = new ArrayList<>(total + 8);
        final Set<Integer> all = new HashSet<>(total);
        for (int idx = 0; idx < total; ++idx) {
            if (idx < inputs) {
                out.add(this.values.reading(idx));
            } else {
                out.add(this.values.blank(idx));
            }
            all.add(idx);
        }
        out.add("int body = 0;");
        out.add(String.format("final %s out;", this.values.type(this.answer())));
        out.add("while (true) {");
        final List<Body> bodies = this.program.bodies();
        for (int idx = 0; idx < bodies.size(); ++idx) {
            out.add(JavaAtom.branch(idx, bodies.size()));
            out.addAll(this.ran(bodies.get(idx).protocol(), all));
        }
        out.add("    }");
        out.add("}");
        out.add("return new Data.ToPhi(out);");
        return out;
    }

    private static String branch(final int idx, final int count) {
        final String out;
        if (idx == 0) {
            out = "    if (body == 0) {";
        } else if (idx == count - 1) {
            out = "    } else {";
        } else {
            out = String.format("    } else if (body == %d) {", idx);
        }
        return out;
    }

    private List<String> ran(final Protocol proto, final Set<Integer> all) {
        final String pad = "        ";
        final List<String> out = this.computed(proto, pad, all, "");
        if (proto.again().isEmpty()) {
            if (!proto.carrier().isEmpty()) {
                out.add(
                    String.format("%sout = %s;", pad, this.values.expression(proto.answer()))
                );
                out.add(String.format("%sbreak;", pad));
            }
        } else {
            out.addAll(this.rebound(proto.target(), proto.again(), pad));
        }
        return out;
    }

    private String answer() {
        this.program.carrier();
        return this.program.bodies().stream()
            .map(Body::protocol)
            .filter(proto -> !proto.carrier().isEmpty())
            .findFirst().get().answer();
    }

    private List<String> computed(final Protocol proto, final String pad,
        final Set<Integer> above, final String exit) {
        final List<String> out = new ArrayList<>(proto.moves().size());
        final SortedSet<Integer> here = new Reads(proto).own(above);
        for (final Integer index : here) {
            out.add(String.format("%sfinal %s", pad, this.values.reading(index)));
        }
        final Set<Integer> known = new HashSet<>(above);
        known.addAll(here);
        for (final Step step : proto.moves()) {
            if (step.branches().isEmpty()) {
                out.add(
                    String.format(
                        "%sfinal %s %s = %s;",
                        pad, this.values.type(String.format("sym:%s", step.label())),
                        step.label(), this.values.applied(step)
                    )
                );
            } else {
                out.addAll(this.forked(step, pad, known, exit));
            }
        }
        return out;
    }

    private List<String> forked(final Step step, final String pad,
        final Set<Integer> known, final String exit) {
        final String test = step.keys().get(0);
        if (!"bool".equals(this.values.forma(test))) {
            throw new IllegalStateException(
                String.format(
                    "The condition '%s' of the fork '%s' does not carry a bool",
                    test, step.label()
                )
            );
        }
        final String inner = String.format("%s    ", pad);
        final List<String> out = new ArrayList<>(8);
        if (!step.label().equals(exit) && !step.forma().isEmpty()) {
            out.add(
                String.format(
                    "%sfinal %s %s;",
                    pad, this.values.type(String.format("sym:%s", step.label())), step.label()
                )
            );
        }
        out.add(String.format("%sif (%s) {", pad, this.values.expression(test)));
        out.addAll(this.assigned(step.label(), step.branches().get(0), inner, known, exit));
        out.add(String.format("%s} else {", pad));
        out.addAll(this.assigned(step.label(), step.branches().get(1), inner, known, exit));
        out.add(String.format("%s}", pad));
        return out;
    }

    private List<String> assigned(final String label, final Protocol arm,
        final String pad, final Set<Integer> known, final String exit) {
        final List<String> out = this.computed(arm, pad, known, exit);
        if (arm.again().isEmpty()) {
            out.add(
                String.format("%s%s = %s;", pad, label, this.values.expression(arm.answer()))
            );
            if (label.equals(exit)) {
                out.add(String.format("%sbreak;", pad));
            }
        } else {
            out.addAll(this.rebound(arm.target(), arm.again(), pad));
        }
        return out;
    }

    private List<String> rebound(final String target, final List<String> keys,
        final String pad) {
        final Body body = this.program.body(target);
        if (keys.size() != body.formas().size()) {
            throw new IllegalStateException(
                String.format(
                    "The repeat hands %d values to %d voids", keys.size(), body.formas().size()
                )
            );
        }
        final List<String> out = new ArrayList<>(keys.size() * 2 + 2);
        final List<String> later = new ArrayList<>(keys.size() + 1);
        for (int idx = 0; idx < keys.size(); ++idx) {
            final String key = keys.get(idx);
            final String local = String.format("v%d", body.offset() + idx);
            final String type = this.values.type(String.format("sym:%s", local));
            if (!type.equals(this.values.type(key))) {
                throw new IllegalStateException(
                    String.format(
                        "The repeat hands '%s' to the void '%s', which is a %s",
                        key, local, type
                    )
                );
            }
            if (key.equals(String.format("sym:%s", local))) {
                continue;
            }
            if (JavaAtom.inside(key, body)) {
                out.add(
                    String.format(
                        "%sfinal %s r%d = %s;", pad, type, body.offset() + idx,
                        this.values.expression(key)
                    )
                );
                later.add(String.format("%s%s = r%d;", pad, local, body.offset() + idx));
            } else {
                later.add(String.format("%s%s = %s;", pad, local, this.values.expression(key)));
            }
        }
        out.addAll(later);
        if (this.program.bodies().size() > 1) {
            out.add(String.format("%sbody = %d;", pad, this.program.index(target)));
        }
        out.add(String.format("%scontinue;", pad));
        return out;
    }

    private static boolean inside(final String key, final Body body) {
        boolean out = false;
        if (key.startsWith("sym:v")) {
            final int idx = Integer.parseInt(key.substring(5));
            out = idx >= body.offset() && idx < body.offset() + body.formas().size();
        }
        return out;
    }
}
