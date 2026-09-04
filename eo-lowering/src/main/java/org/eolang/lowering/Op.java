/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * One lowerable operation, looked up by the λ name phino reports.
 *
 * <p>This is the one table of atom knowledge this module keeps, and it is
 * the semantic definition of the lowering target: an operation absent
 * from the {@code ops.tsv} resource cannot become a step of a protocol,
 * however well phino evaluates it. Each row binds a λ name to the method
 * that dispatches it, the forma of its receiver, the forma of its value,
 * and the names of its arguments in their positional order, which is how
 * a record naming its bindings meets an XMIR application naming the same
 * bindings {@code α0}, {@code α1} and so on.</p>
 *
 * @since 0.76.0
 */
public final class Op {

    /**
     * The λ name, such as {@code L_number_plus}.
     */
    private final String lambda;

    /**
     * Ctor.
     * @param name The λ name, such as {@code L_number_plus}
     */
    public Op(final String name) {
        this.lambda = name;
    }

    /**
     * Whether this operation may become a step of a protocol.
     * @return True if the table has a row for the λ name
     */
    public boolean listed() {
        return this.rows().stream().anyMatch(row -> row[0].equals(this.lambda));
    }

    /**
     * The method that dispatches this operation.
     * @return The name, such as {@code plus}
     */
    public String method() {
        return this.row()[1];
    }

    /**
     * The forma of the receiver.
     * @return Either {@code number} or {@code bytes}
     */
    public String carrier() {
        return this.row()[2];
    }

    /**
     * The forma of the value.
     * @return One of {@code number}, {@code bool}, {@code bytes}
     */
    public String forma() {
        return this.row()[3];
    }

    /**
     * The Java rendering of this operation, as a format string whose
     * positional arguments are the receiver and then the arguments.
     * An operation may have no rendering — {@code right} and {@code slice}
     * coerce their bounds the way only the hand-written atoms can, and
     * {@code eq} renders by the forma of its operands in {@code JavaAtom} —
     * and such an operation otherwise reduces fine but refuses to become
     * Java.
     * @return A format, such as {@code %1$s + %2$s}
     */
    public String java() {
        final String[] row = this.row();
        if (row.length < 6 || row[5].isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "The atom '%s' has no faithful Java rendering",
                    this.lambda
                )
            );
        }
        return row[5];
    }

    /**
     * The names of the arguments, in their positional order.
     * @return The names, such as {@code start} and {@code len}
     */
    public List<String> args() {
        final String[] row = this.row();
        final List<String> out;
        if (row.length > 4 && !row[4].isEmpty()) {
            out = Arrays.asList(row[4].split(","));
        } else {
            out = Collections.emptyList();
        }
        return out;
    }

    private String[] row() {
        String[] out = null;
        for (final String[] row : this.rows()) {
            if (row[0].equals(this.lambda)) {
                out = row;
                break;
            }
        }
        if (out == null) {
            throw new IllegalStateException(
                String.format(
                    "The atom '%s' is not among the lowerable operations",
                    this.lambda
                )
            );
        }
        return out;
    }

    private List<String[]> rows() {
        try (InputStream stream = this.getClass().getResourceAsStream("ops.tsv")) {
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8)
                .lines()
                .filter(line -> !line.isEmpty())
                .map(line -> line.split("\t", -1))
                .collect(Collectors.toList());
        } catch (final IOException ex) {
            throw new IllegalStateException(
                "Failed to read ops.tsv from classpath", ex
            );
        }
    }
}
