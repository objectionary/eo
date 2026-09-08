/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Optional;

/**
 * An application whose trailing tuple is compacted onto its head line as
 * a {@code *N} marker.
 *
 * <p>The ordinary {@code seq *} idiom is a tuple applied as the sole
 * argument of an object. Rendered verbosely it becomes {@code seq} on
 * one line, a lone {@code *} on the next, and the elements one level
 * deeper still — a line taller and an indent wider than the source a
 * human writes. This keeps the {@code *} at the tail of the head's line
 * ({@code seq *}) and pulls the elements up one level, mirroring the
 * hybrid inline-phi form (issues #5594, #5615). When the tuple follows
 * {@code N >= 1} leading positional arguments, the compact-tuple marker
 * {@code *N} (§3.9) carries the count on the head's line
 * ({@code sprintf *1}) and every argument — the leading ones and the
 * tuple's elements alike — becomes an indented sibling (issue #5648).
 * Either way the shape is the same tree as the verbose one, with no
 * before-star ambiguity.</p>
 *
 * <p>It applies to a plain (non-formation, non-reversed) application
 * whose last child is a non-empty, unnamed star. When the star is the
 * sole child ({@code N == 0}), the bare {@code *} absorbs the indented
 * siblings into the tuple and the head must be a plain base, not a
 * dotted method dispatch: after {@code "literal".printf *} the parser
 * reads a complete application with an empty tuple and drops the
 * indented element, so {@link Node#tuply()} bars that case (issues #5622,
 * #5624). The {@code *N} marker ({@code N >= 1}) sits on the head line
 * rather than being glued after arguments, so it round-trips after a
 * dotted dispatch too ({@code string.sprintf *1}). The genuinely
 * ambiguous before-star form {@code head args *} is never produced. The
 * result is only a candidate: the penalty vote keeps it only when it beats
 * the plain vertical and horizontal renderings, so a short tuple whose
 * one-line {@code * 1 2} form is no worse than the vertical one stays
 * inline as that bare tuple rather than the hybrid star.</p>
 *
 * @since 0.57.0
 */
final class Starred {

    /**
     * The application to compact.
     */
    private final Node node;

    /**
     * Ctor.
     *
     * @param source The application to compact
     */
    Starred(final Node source) {
        this.node = source;
    }

    /**
     * Render the compacted application at the given indentation level.
     *
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block, or empty if the trailing-star form doesn't apply
     */
    Optional<String> print(final Style style, final int indent) {
        final Optional<String> result;
        if (this.node.tuply()) {
            result = Optional.of(this.node.glued().vertical(style, indent));
        } else {
            result = Optional.empty();
        }
        return result;
    }
}
