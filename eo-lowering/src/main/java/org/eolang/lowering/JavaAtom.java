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
 * through temporaries wherever a value names a void, and continues. The
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
     * The protocol to render.
     */
    private final Protocol protocol;

    /**
     * The voids of the fragment: names to formas, in declaration order.
     */
    private final Map<String, String> voids;

    /**
     * The spelling of the values.
     */
    private final Rendering values;

    /**
     * Ctor.
     * @param proto The protocol to render
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public JavaAtom(final Protocol proto, final Map<String, String> inputs) {
        this.protocol = proto;
        this.voids = inputs;
        this.values = new Rendering(proto, inputs);
    }

    /**
     * The body of the {@code lambda()} method.
     * @return Java statements, one per line, without a trailing newline
     */
    public String text() {
        if ("string".equals(this.protocol.carrier())) {
            throw new IllegalStateException(
                "A string answer cannot be handed over, since Data.ToPhi makes bytes of a byte array"
            );
        }
        final List<String> lines;
        if (this.protocol.repeats()) {
            lines = this.looped();
        } else {
            lines = this.computed(this.protocol, "", Collections.emptySet(), "");
        }
        lines.add(
            String.format(
                "return new Data.ToPhi(%s);",
                this.values.expression(this.protocol.answer())
            )
        );
        return lines.stream()
            .map(line -> String.format("        %s", line))
            .collect(Collectors.joining(System.lineSeparator()));
    }

    private List<String> looped() {
        final String exit = this.protocol.answer();
        if (!exit.startsWith("sym:s") || this.values.step(exit.substring(4)).branches().isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "A program that repeats must answer through a fork, but it answers '%s'",
                    exit
                )
            );
        }
        final Step last = this.values.step(exit.substring(4));
        final List<String> out = new ArrayList<>(this.voids.size() + 4);
        final Set<Integer> all = new HashSet<>(this.voids.size());
        for (int idx = 0; idx < this.voids.size(); ++idx) {
            out.add(this.values.reading(idx));
            all.add(idx);
        }
        out.add(String.format("final %s %s;", this.values.type(exit), last.label()));
        out.add("while (true) {");
        out.addAll(this.computed(this.protocol, "    ", all, last.label()));
        out.add("}");
        return out;
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
        if (!step.label().equals(exit)) {
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
            out.addAll(this.rebound(arm.again(), pad));
        }
        return out;
    }

    private List<String> rebound(final List<String> keys, final String pad) {
        final List<String> names = new ArrayList<>(this.voids.keySet());
        if (keys.size() != names.size()) {
            throw new IllegalStateException(
                String.format(
                    "The repeat hands %d values to %d voids", keys.size(), names.size()
                )
            );
        }
        final List<String> out = new ArrayList<>(keys.size() * 2 + 1);
        final List<String> later = new ArrayList<>(keys.size());
        for (int idx = 0; idx < keys.size(); ++idx) {
            final String key = keys.get(idx);
            final String type = this.values.type(String.format("sym:v%d", idx));
            if (!type.equals(this.values.type(key))) {
                throw new IllegalStateException(
                    String.format(
                        "The repeat hands '%s' to the void '%s', which is a %s",
                        key, names.get(idx), type
                    )
                );
            }
            if (key.equals(String.format("sym:v%d", idx))) {
                continue;
            }
            if (key.startsWith("sym:v")) {
                out.add(
                    String.format(
                        "%sfinal %s r%d = %s;", pad, type, idx, this.values.expression(key)
                    )
                );
                later.add(String.format("%sv%d = r%d;", pad, idx, idx));
            } else {
                later.add(String.format("%sv%d = %s;", pad, idx, this.values.expression(key)));
            }
        }
        out.addAll(later);
        out.add(String.format("%scontinue;", pad));
        return out;
    }
}
