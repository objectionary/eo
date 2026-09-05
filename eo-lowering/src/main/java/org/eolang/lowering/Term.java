/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;
import java.util.Optional;

/**
 * One node of the tree a reduction works on.
 *
 * <p>The tree mirrors the XMIR fragment being lowered: a {@link Site} for
 * every application, a {@link Literal} for every datum, a {@link Symbol}
 * for every value that is not known yet — a void of the fragment, or a
 * step already minted. The tree renders itself into phi text for each
 * partial run, and rewrites itself as the records come back: a site
 * matching a fired record becomes the literal it computed, a site
 * matching a parked record becomes the symbol of a new step. Values
 * carry a key naming their identity, and matching is key equality, so
 * identical sites collapse into one step wherever they stand.</p>
 *
 * @since 0.76.0
 */
public interface Term {

    /**
     * The term, in phi syntax.
     * @return The text, ready to stand inside an expression
     */
    String phi();

    /**
     * The identity of the value this term stands for.
     * @return A key such as {@code sym:s1} or {@code number:40-14-...}, empty for a site
     */
    String key();

    /**
     * Whether any site of this tree matches the shape.
     * @param shape The shape of a recorded evaluation
     * @return True if {@link #swapped(Shape, Term)} would rewrite something
     */
    boolean matches(Shape shape);

    /**
     * The arguments of the first site of this tree matching the shape.
     * @param shape The shape of a recorded evaluation
     * @return The bindings of that site, or empty when no site matches
     */
    Optional<List<Binding>> arguments(Shape shape);

    /**
     * This tree with every site matching the shape replaced.
     * @param shape The shape of a recorded evaluation
     * @param swap The term to stand where a matching site stood
     * @return The rewritten tree, or this one if nothing matched
     */
    Term swapped(Shape shape, Term swap);
}
