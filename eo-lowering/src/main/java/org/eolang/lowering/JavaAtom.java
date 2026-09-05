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
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Java body of one lowered fragment.
 *
 * <p>A protocol is a program of steps, so its Java is one statement per
 * step: one local per void any step reads, dataized through the public
 * runtime API, one local per application, rendered by the format the
 * {@link Op} table holds for its atom, one blank final per fork,
 * assigned at the end of each of its two arms, whose own steps sit
 * inside the arm's block and so compute only when the arm is taken, and
 * one return handing the answer to {@code Data.ToPhi}. A void is read
 * at the top of the innermost block that reaches every use of it: at the
 * top of the body when a step outside every fork reads it, or both arms
 * of one fork do, and at the top of an arm when that arm alone does, so
 * that an argument a guard protects is never forced while the guard
 * holds it back. A string is bytes here: its Δ is the very UTF-8
 * sequence the byte atoms it reaches through {@code φ} operate on, so a
 * string void is read as a {@code byte[]} and meets a bytes carrier as
 * one. The text is exactly what the {@code lambda()} of the generated
 * atom class holds, indented for that spot, and it is the content the
 * sidecar file is named after. A protocol that steps outside
 * what the table renders — an operation with no Java column, a void of a
 * forma the runtime cannot hand over, an answer whose carrier no
 * {@code Data.ToPhi} argument names — is refused, and the caller treats
 * the refusal as one fragment staying unlowered.</p>
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
     * Ctor.
     * @param proto The protocol to render
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public JavaAtom(final Protocol proto, final Map<String, String> inputs) {
        this.protocol = proto;
        this.voids = inputs;
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
        final List<String> lines = this.computed(
            this.protocol, "", Collections.emptySet()
        );
        lines.add(
            String.format(
                "return new Data.ToPhi(%s);",
                JavaAtom.expression(this.protocol.answer())
            )
        );
        return lines.stream()
            .map(line -> String.format("        %s", line))
            .collect(Collectors.joining(System.lineSeparator()));
    }

    private static Stream<Protocol> unfolded(final Protocol proto) {
        return Stream.concat(
            Stream.of(proto),
            proto.moves().stream()
                .flatMap(step -> step.branches().stream())
                .flatMap(JavaAtom::unfolded)
        );
    }

    private String dataized(final int index) {
        final List<String> names = new ArrayList<>(this.voids.keySet());
        if (index >= names.size()) {
            throw new IllegalStateException(
                String.format("The protocol reads void #%d, which the fragment lacks", index)
            );
        }
        final String name = names.get(index);
        return JavaAtom.reading(index, name, this.voids.get(name));
    }

    private static String reading(final int index, final String name, final String forma) {
        final String out;
        if ("number".equals(forma)) {
            out = String.format(
                "final double v%d = new Dataized(this.take(\"%s\")).asNumber();",
                index, name
            );
        } else if ("bool".equals(forma)) {
            out = String.format(
                "final boolean v%d = new Dataized(this.take(\"%s\")).asBool();",
                index, name
            );
        } else if ("bytes".equals(JavaAtom.carried(forma))) {
            out = String.format(
                "final byte[] v%d = new Dataized(this.take(\"%s\")).take();",
                index, name
            );
        } else {
            throw new IllegalStateException(
                String.format("The void '%s' of forma '%s' cannot be read in Java", name, forma)
            );
        }
        return out;
    }

    private List<String> computed(final Protocol proto, final String pad,
        final Set<Integer> above) {
        final List<String> out = new ArrayList<>(proto.moves().size());
        final SortedSet<Integer> here = new Reads(proto).own(above);
        for (final Integer index : here) {
            out.add(String.format("%s%s", pad, this.dataized(index)));
        }
        final Set<Integer> known = new HashSet<>(above);
        known.addAll(here);
        for (final Step step : proto.moves()) {
            if (step.branches().isEmpty()) {
                out.add(
                    String.format(
                        "%sfinal %s %s = %s;",
                        pad, JavaAtom.typed(step.forma()), step.label(), this.applied(step)
                    )
                );
            } else {
                out.addAll(this.forked(step, pad, known));
            }
        }
        return out;
    }

    private List<String> forked(final Step step, final String pad, final Set<Integer> known) {
        final String test = step.keys().get(0);
        if (!"bool".equals(this.forma(test))) {
            throw new IllegalStateException(
                String.format(
                    "The condition '%s' of the fork '%s' does not carry a bool",
                    test, step.label()
                )
            );
        }
        final String inner = String.format("%s    ", pad);
        final List<String> out = new ArrayList<>(8);
        out.add(
            String.format(
                "%sfinal %s %s;", pad, JavaAtom.typed(JavaAtom.carried(step.forma())), step.label()
            )
        );
        out.add(String.format("%sif (%s) {", pad, JavaAtom.expression(test)));
        out.addAll(this.assigned(step.label(), step.branches().get(0), inner, known));
        out.add(String.format("%s} else {", pad));
        out.addAll(this.assigned(step.label(), step.branches().get(1), inner, known));
        out.add(String.format("%s}", pad));
        return out;
    }

    private List<String> assigned(final String label, final Protocol arm,
        final String pad, final Set<Integer> known) {
        final List<String> out = this.computed(arm, pad, known);
        out.add(String.format("%s%s = %s;", pad, label, JavaAtom.expression(arm.answer())));
        return out;
    }

    private String applied(final Step step) {
        final Op operation = new Op(step.atom());
        final String out;
        if ("eq".equals(operation.method())) {
            out = this.compared(step);
        } else {
            final String format = operation.java();
            for (final String key : step.keys()) {
                if (!operation.carrier().equals(this.forma(key))) {
                    throw new IllegalStateException(
                        String.format(
                            "The operand '%s' of '%s' does not carry a %s",
                            key, step.atom(), operation.carrier()
                        )
                    );
                }
            }
            out = String.format(
                format,
                step.keys().stream().map(JavaAtom::expression).toArray(Object[]::new)
            );
        }
        return out;
    }

    private String compared(final Step step) {
        final String kinds = step.keys().stream()
            .map(this::forma)
            .distinct()
            .collect(Collectors.joining(","));
        final List<String> sides = step.keys().stream()
            .map(JavaAtom::expression)
            .collect(Collectors.toList());
        final String out;
        if ("number".equals(kinds)) {
            out = String.format(
                "Double.doubleToRawLongBits(%s) == Double.doubleToRawLongBits(%s)",
                sides.get(0), sides.get(1)
            );
        } else if ("bytes".equals(kinds)) {
            out = String.format(
                "java.util.Arrays.equals(%s, %s)",
                sides.get(0), sides.get(1)
            );
        } else if ("bool".equals(kinds)) {
            out = String.format("%s == %s", sides.get(0), sides.get(1));
        } else {
            throw new IllegalStateException(
                String.format(
                    "The equality '%s' mixes the formas '%s' and cannot render",
                    step.label(), kinds
                )
            );
        }
        return out;
    }

    private String forma(final String key) {
        final String[] parts = key.split(":", 2);
        final String out;
        if ("sym".equals(parts[0])) {
            if (parts[1].charAt(0) == 'v') {
                out = this.voids.get(
                    new ArrayList<>(this.voids.keySet())
                        .get(Integer.parseInt(parts[1].substring(1)))
                );
            } else {
                out = this.move(parts[1]).forma();
            }
        } else {
            out = parts[0];
        }
        return JavaAtom.carried(out);
    }

    private static String carried(final String forma) {
        final String out;
        if ("string".equals(forma)) {
            out = "bytes";
        } else {
            out = forma;
        }
        return out;
    }

    private Step move(final String label) {
        final Optional<Step> found = JavaAtom.unfolded(this.protocol)
            .flatMap(proto -> proto.moves().stream())
            .filter(step -> step.label().equals(label))
            .findFirst();
        if (!found.isPresent()) {
            throw new IllegalStateException(
                String.format("The protocol has no step '%s'", label)
            );
        }
        return found.get();
    }

    private static String typed(final String forma) {
        final String out;
        if ("number".equals(forma)) {
            out = "double";
        } else if ("bool".equals(forma)) {
            out = "boolean";
        } else if ("bytes".equals(forma)) {
            out = "byte[]";
        } else {
            throw new IllegalStateException(
                String.format("The forma '%s' has no Java type to carry it", forma)
            );
        }
        return out;
    }

    private static String expression(final String key) {
        final String out;
        final String[] parts = key.split(":", 2);
        if ("sym".equals(parts[0])) {
            out = parts[1];
        } else if ("number".equals(parts[0])) {
            final String hex = parts[1].replace("-", "");
            if (hex.length() != 16) {
                throw new IllegalStateException(
                    String.format("The number '%s' is not eight bytes", parts[1])
                );
            }
            out = String.format("Double.longBitsToDouble(0x%sL)", hex);
        } else if ("bool".equals(parts[0])) {
            if ("FF-".equals(parts[1])) {
                out = "true";
            } else if ("00-".equals(parts[1])) {
                out = "false";
            } else {
                throw new IllegalStateException(
                    String.format("The bool '%s' is not one byte", parts[1])
                );
            }
        } else if ("bytes".equals(JavaAtom.carried(parts[0]))) {
            out = JavaAtom.array(parts[1]);
        } else {
            throw new IllegalStateException(
                String.format("The operand '%s' has no Java expression", key)
            );
        }
        return out;
    }

    private static String array(final String dashed) {
        final List<String> cells = new ArrayList<>(0);
        for (final String pair : dashed.split("-", -1)) {
            if (!pair.isEmpty()) {
                cells.add(String.format("(byte) 0x%s", pair));
            }
        }
        final String out;
        if (cells.isEmpty()) {
            out = "new byte[0]";
        } else {
            out = String.format("new byte[] {%s}", String.join(", ", cells));
        }
        return out;
    }
}
