/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Optional;

/**
 * A formation whose only attribute is its {@code φ} decoratee, collapsed
 * onto the decoratee's own line.
 *
 * <p>A formation with a {@code [params]} head collapses to
 * {@code <phi> > [params] > name} (R-3.10.8 §4.5). A test attribute
 * with no void params has an empty head — the head template renders
 * it through the {@code ++> name} shorthand — so it collapses to
 * {@code <phi> ++> name} (R-3.10.8 / R-6.3.6, issue #5567), the
 * decoratee sitting in front of the {@code ++>} marker instead of a
 * bracket. An empty head occurs only for that no-void test attribute,
 * so it selects the shorthand separator here.</p>
 *
 * <p>The decoratee itself is inlined through {@link Node#flat()}. When
 * that fails because the decoratee's arguments must go vertical (a
 * tuple, a nested formation), a hybrid multi-line form is returned
 * instead: the decoratee's head kept in front of the marker
 * ({@code head ++> name} or {@code head > [params] > name}) with the
 * arguments laid out beneath, mirroring the ordinary {@code head > name}
 * plus vertical-args layout and saving one line and one indent level
 * over the verbose shape (issue #5594); when those arguments are a lone
 * tuple the star is glued onto the head line too ({@link Node#hybrid}). The
 * flat one-liner and the hybrid are both built and the lower-penalty of
 * the two is returned, rather than discriminating between them by whether
 * the one-liner fits the {@code WIDTH} limit: the hybrid drops the
 * {@code @} (saving its {@code PHI} charge) and lifts the arguments one
 * indent level, so it is often cheaper even when the one-liner fits, and a
 * width check would hide it whenever the one-liner did not overflow (issue
 * #5700, the residual case #5635 left open). When the one-liner cannot be
 * built at all, the hybrid is used unconditionally. Either way the result
 * is only a candidate — the penalty check in {@link Node#print} keeps it
 * only when it beats the plain vertical rendering. A formation decoratee
 * (its bindings are vertical, not arguments) has no hybrid form, so it
 * yields empty and keeps the verbose layout. A reversed dispatch does get
 * the hybrid whether it carries just its receiver ({@code not.}) or also
 * arguments ({@code if.} with its branches): unlike {@link Node#flat()},
 * which rejects a receiver-only reversed dispatch because it has no
 * horizontal one-line spelling, the hybrid keeps the dispatch on the head
 * line and lays the receiver out beneath — a shape {@code LnOnlyPhi}
 * accepts, its {@code bare()} leaving the φ {@code Openness.OPEN} so the
 * deeper line attaches as the receiver, with no minimum argument count on
 * close (issue #5954).</p>
 *
 * <p>The hybrid is withheld when any line in the decoratee's whole
 * subtree carries a name suffix ({@code [left] >>}, {@code malloc.for >
 * [b]}, {@code b.put > [m] >>}). The decoratee's subtree becomes the body
 * of the collapsed only-phi formation, and an only-phi formation may hold
 * nothing but its {@code φ} decoratee, so a named line anywhere within it
 * fails to parse with "an auto-named attribute cannot be a named attribute
 * of an only-phi formation, which binds only its φ decoratee" (issues
 * #5598, #5604). A direct child is not enough to check: the offending line
 * may be nested deeper — inside a tuple, a dispatch, or an application —
 * where a shallow guard would miss it (issue #5604). Keeping the verbose
 * {@code  > @} layout gives the decoratee its own scope, where named
 * arguments are legal.</p>
 *
 * @since 0.57.0
 */
final class Phi {

    /**
     * The rendered head of the formation ({@code [params]}, or empty for
     * a test attribute with no void params).
     */
    private final String base;

    /**
     * The rendered suffix of the formation, possibly empty.
     */
    private final String tail;

    /**
     * The {@code φ} decoratee, the formation's only binding.
     */
    private final Node decoratee;

    /**
     * Ctor.
     *
     * @param head The rendered head of the formation
     * @param suffix The rendered suffix of the formation
     * @param phi The {@code φ} decoratee
     */
    Phi(final String head, final String suffix, final Node phi) {
        this.base = head;
        this.tail = suffix;
        this.decoratee = phi;
    }

    /**
     * Render the collapsed formation at the given indentation level.
     *
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block, or empty if the inline-phi form doesn't apply
     */
    Optional<String> print(final Style style, final int indent) {
        final String middle;
        if (this.base.isEmpty()) {
            middle = " ";
        } else {
            middle = " > ".concat(this.base);
        }
        final String marker = middle.concat(this.tail);
        final Optional<String> flat = this.decoratee.bare().flat().map(
            inlined -> style.indent(indent).concat(inlined).concat(marker)
        );
        final Optional<String> result;
        if (this.decoratee.applied() && this.decoratee.anonymous()) {
            final String hybrid = this.decoratee.hybrid(marker).vertical(style, indent);
            result = Optional.of(
                flat.filter(
                    line -> style.points(line) <= style.points(hybrid)
                ).orElse(hybrid)
            );
        } else {
            result = flat;
        }
        return result;
    }
}
