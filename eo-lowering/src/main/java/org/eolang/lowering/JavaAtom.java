/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * The Java body of one lowered fragment.
 *
 * <p>A protocol is a straight-line program, so its Java is a straight
 * line too: one local per void a step reads, dataized through the public
 * runtime API, one local per step, rendered by the format the {@link Op}
 * table holds for its atom, and one return handing the answer to
 * {@code Data.ToPhi}. The text is exactly what the {@code lambda()} of
 * the generated atom class holds, indented for that spot, and it is the
 * content the sidecar file is named after. A protocol that steps outside
 * what the table renders — an operation with no Java column, a void of a
 * forma the runtime cannot hand over — is refused, and the caller treats
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
        final List<String> names = new ArrayList<>(this.voids.keySet());
        final List<String> lines = new ArrayList<>(names.size());
        for (final Integer index : this.used()) {
            if (index >= names.size()) {
                throw new IllegalStateException(
                    String.format("The protocol reads void #%d, which the fragment lacks", index)
                );
            }
            final String name = names.get(index);
            lines.add(JavaAtom.reading(index, name, this.voids.get(name)));
        }
        for (final Step step : this.protocol.moves()) {
            lines.add(this.computed(step));
        }
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

    private SortedSet<Integer> used() {
        final List<String> keys = new ArrayList<>(0);
        for (final Step step : this.protocol.moves()) {
            keys.addAll(step.keys());
        }
        keys.add(this.protocol.answer());
        final SortedSet<Integer> out = new TreeSet<>();
        for (final String key : keys) {
            if (key.startsWith("sym:v")) {
                out.add(Integer.parseInt(key.substring(5)));
            }
        }
        return out;
    }

    private static String reading(final int index, final String name, final String forma) {
        final String out;
        if ("number".equals(forma)) {
            out = String.format(
                "final double v%d = new Dataized(this.take(\"%s\")).asNumber();",
                index, name
            );
        } else if ("bytes".equals(forma)) {
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

    private String computed(final Step step) {
        final Op operation = new Op(step.atom());
        return String.format(
            "final %s %s = %s;",
            JavaAtom.typed(operation.forma()),
            step.label(),
            this.applied(step, operation)
        );
    }

    private String applied(final Step step, final Op operation) {
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
                out = new Op(this.move(parts[1]).atom()).forma();
            }
        } else {
            out = parts[0];
        }
        return out;
    }

    private Step move(final String label) {
        Step found = null;
        for (final Step step : this.protocol.moves()) {
            if (step.label().equals(label)) {
                found = step;
                break;
            }
        }
        if (found == null) {
            throw new IllegalStateException(
                String.format("The protocol has no step '%s'", label)
            );
        }
        return found;
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
            if ("01-".equals(parts[1])) {
                out = "true";
            } else if ("00-".equals(parts[1])) {
                out = "false";
            } else {
                throw new IllegalStateException(
                    String.format("The bool '%s' is not one byte", parts[1])
                );
            }
        } else if ("bytes".equals(parts[0])) {
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
