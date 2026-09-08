/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The arguments of an application, laid out on one line.
 *
 * <p>Each argument spells itself through {@link Node#inlined()}, which
 * brackets the ones that apply arguments of their own (a real application
 * such as {@code 5.plus 3}) and leaves single tokens bare. The whole list
 * is inlinable only when every argument is, so one argument that has no
 * one-line spelling — a formation, a named attribute, a receiver-only
 * reversed dispatch — makes the join yield empty and sends the caller
 * back to a vertical layout.</p>
 *
 * @since 0.57.0
 */
final class Arguments {

    /**
     * The arguments, in order.
     */
    private final List<Node> args;

    /**
     * Ctor.
     *
     * @param nodes The arguments, in order
     */
    Arguments(final List<Node> nodes) {
        this.args = nodes;
    }

    /**
     * Join the arguments into one space-separated string.
     *
     * @return The inlined string, or empty if any argument cannot be inlined
     */
    Optional<String> joined() {
        final List<String> parts = new ArrayList<>(this.args.size());
        boolean whole = true;
        for (final Node arg : this.args) {
            final Optional<String> one = arg.inlined();
            if (!one.isPresent()) {
                whole = false;
                break;
            }
            parts.add(one.get());
        }
        final Optional<String> result;
        if (whole) {
            result = Optional.of(String.join(" ", parts));
        } else {
            result = Optional.empty();
        }
        return result;
    }
}
