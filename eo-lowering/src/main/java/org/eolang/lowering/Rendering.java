/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Java spelling of the values one protocol computes.
 *
 * <p>Every operand key has one Java expression: a symbol is the local
 * named after it, a number literal is the double its eight bytes
 * encode, a bool is {@code true} or {@code false}, and bytes or a string
 * are a byte array. Every forma has one Java type, with a string carried
 * as bytes, since its Δ is the very UTF-8 sequence the byte atoms it
 * reaches through {@code φ} operate on. The value of an application
 * comes from the format the {@link Op} table holds for its atom, except
 * an equality, which compares by the forma of its operands; and a void
 * is read through the public runtime API. The forma of a key is looked
 * up in the voids of the program or in the steps of its bodies,
 * nested arms included. Whatever the table cannot spell — an operation
 * with no Java column, a void of a forma the runtime cannot hand over,
 * an operand of a forma the atom does not take — is refused.</p>
 *
 * @since 0.76.0
 */
public final class Rendering {

    /**
     * The program.
     */
    private final Program program;

    /**
     * Ctor, for a program of one body.
     * @param proto The protocol
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public Rendering(final Protocol proto, final Map<String, String> inputs) {
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
     * @param plan The program
     */
    public Rendering(final Program plan) {
        this.program = plan;
    }

    /**
     * The declaration reading one void of the formation, without {@code final}.
     * @param index The index of the void
     * @return A statement such as {@code double v0 = new Dataized(this.take("x")).asNumber();}
     */
    public String reading(final int index) {
        final List<String> names = new ArrayList<>(this.program.inputs().keySet());
        if (index >= names.size()) {
            throw new IllegalStateException(
                String.format("The protocol reads void #%d, which the fragment lacks", index)
            );
        }
        final String name = names.get(index);
        final String forma = this.program.inputs().get(name);
        final String out;
        if ("number".equals(forma)) {
            out = String.format(
                "double v%d = new Dataized(this.take(\"%s\")).asNumber();", index, name
            );
        } else if ("bool".equals(forma)) {
            out = String.format(
                "boolean v%d = new Dataized(this.take(\"%s\")).asBool();", index, name
            );
        } else if ("bytes".equals(Rendering.carried(forma))) {
            out = String.format(
                "byte[] v%d = new Dataized(this.take(\"%s\")).take();", index, name
            );
        } else {
            throw new IllegalStateException(
                String.format("The void '%s' of forma '%s' cannot be read in Java", name, forma)
            );
        }
        return out;
    }

    /**
     * The declaration of one void of a resumed body, blank until a repeat
     * hands it a value, without {@code final}.
     * @param index The index of the void
     * @return A statement such as {@code double v3 = 0.0;}
     */
    public String blank(final int index) {
        final String type = this.type(String.format("sym:v%d", index));
        final String out;
        if ("double".equals(type)) {
            out = String.format("double v%d = 0.0;", index);
        } else if ("boolean".equals(type)) {
            out = String.format("boolean v%d = false;", index);
        } else {
            out = String.format("byte[] v%d = new byte[0];", index);
        }
        return out;
    }

    /**
     * The value of one application.
     * @param step The application
     * @return A Java expression over the locals of its operands
     */
    public String applied(final Step step) {
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
                step.keys().stream().map(this::expression).toArray(Object[]::new)
            );
        }
        return out;
    }

    /**
     * The Java type of the value a key names.
     * @param key The key, such as {@code sym:v0} or {@code sym:s2}
     * @return The type, such as {@code double} or {@code byte[]}
     */
    public String type(final String key) {
        return Rendering.typed(this.forma(key), key);
    }

    /**
     * The forma a key carries in Java, with a string carried as bytes.
     * @param key The key, such as {@code sym:v0} or {@code number:40-...}
     * @return The forma, one of {@code number}, {@code bool}, {@code bytes}
     */
    public String forma(final String key) {
        final String[] parts = key.split(":", 2);
        final String out;
        if ("sym".equals(parts[0])) {
            if (parts[1].charAt(0) == 'v') {
                out = this.program.formas().get(Integer.parseInt(parts[1].substring(1)));
            } else {
                out = this.step(parts[1]).forma();
            }
        } else {
            out = parts[0];
        }
        return Rendering.carried(out);
    }

    /**
     * The step with the label, wherever it stands in the protocol.
     * @param label The label, such as {@code s3}
     * @return The step
     */
    public Step step(final String label) {
        final Optional<Step> found = this.program.bodies().stream()
            .map(Body::protocol)
            .flatMap(Rendering::unfolded)
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

    /**
     * The Java expression of a key.
     * @param key The key, such as {@code sym:s1} or {@code bool:FF-}
     * @return The expression, such as {@code s1} or {@code true}
     */
    public String expression(final String key) {
        final String out;
        final String[] parts = key.split(":", 2);
        if ("sym".equals(parts[0])) {
            this.forma(key);
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
        } else if ("bytes".equals(Rendering.carried(parts[0]))) {
            out = Rendering.array(parts[1]);
        } else {
            throw new IllegalStateException(
                String.format("The operand '%s' has no Java expression", key)
            );
        }
        return out;
    }

    private static String typed(final String carrier, final String what) {
        final String out;
        if ("number".equals(carrier)) {
            out = "double";
        } else if ("bool".equals(carrier)) {
            out = "boolean";
        } else if ("bytes".equals(carrier)) {
            out = "byte[]";
        } else {
            throw new IllegalStateException(
                String.format("The value '%s' has no Java type to carry it", what)
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
            .map(this::expression)
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

    private static Stream<Protocol> unfolded(final Protocol proto) {
        return Stream.concat(
            Stream.of(proto),
            proto.moves().stream()
                .flatMap(step -> step.branches().stream())
                .flatMap(Rendering::unfolded)
        );
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
