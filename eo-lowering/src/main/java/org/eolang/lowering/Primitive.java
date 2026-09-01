/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Map;

/**
 * One of the twelve primitive λ-atoms and the forma of its result.
 *
 * <p>All data operations of EO bottom out in twelve atoms: four on
 * {@code number} and eight on {@code bytes}. Their names are disjoint
 * across the two formas, so the name of the outermost dispatch of a
 * folded fragment decides the forma of the value alone: a dataization
 * that succeeded found its method in the tables of
 * {@link MiniDoc}, and only one forma holds a method of that name there.
 * A name outside the twelve is not necessarily impure — it is merely
 * something the tables cannot answer, so a fragment led by it is not
 * worth a dataization attempt.</p>
 *
 * @since 0.76.0
 */
public final class Primitive {

    /**
     * The forma of each primitive's result, by the primitive's name.
     */
    private static final Map<String, String> FORMAS = Map.ofEntries(
        Map.entry("plus", "number"),
        Map.entry("times", "number"),
        Map.entry("div", "number"),
        Map.entry("size", "number"),
        Map.entry("gt", "bool"),
        Map.entry("eq", "bool"),
        Map.entry("and", "bytes"),
        Map.entry("or", "bytes"),
        Map.entry("not", "bytes"),
        Map.entry("concat", "bytes"),
        Map.entry("right", "bytes"),
        Map.entry("slice", "bytes")
    );

    /**
     * The name of the method, without the leading dot.
     */
    private final String name;

    /**
     * Ctor.
     * @param method The name of the method, without the leading dot
     */
    public Primitive(final String method) {
        this.name = method;
    }

    /**
     * Whether this name is one of the twelve.
     * @return True if {@link #forma()} has an answer
     */
    public boolean known() {
        return Primitive.FORMAS.containsKey(this.name);
    }

    /**
     * The forma of the result.
     * @return One of {@code number}, {@code bool}, {@code bytes}
     */
    public String forma() {
        final String found = Primitive.FORMAS.get(this.name);
        if (found == null) {
            throw new IllegalStateException(
                String.format(
                    "The method '%s' is not one of the twelve primitives",
                    this.name
                )
            );
        }
        return found;
    }
}
