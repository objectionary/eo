/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.Collections;
import java.util.Map;

/**
 * Penalty-based pretty printer of EO code.
 *
 * <p>It takes the intermediate "line tree" produced by
 * {@code to-eo-tree.xsl} (an {@code <eo>} element with a {@code
 * <preamble>} and nested {@code <line>} elements), puts the preamble in
 * front and asks the root {@link Node} to print itself in the given
 * {@link Style}. Every object picks its own rendering there, weighing the
 * candidates against each other by {@link Penalty}.</p>
 *
 * @since 0.57.0
 */
final class Pretty {

    /**
     * The default weights: an empty map, so every key uses its fallback.
     */
    private static final Map<PenaltyKey, Integer> DEFAULTS = Collections.emptyMap();

    /**
     * The {@code <eo>} element produced by {@code to-eo-tree.xsl}.
     */
    private final Xnav root;

    /**
     * The style every node is laid out in.
     */
    private final Style style;

    /**
     * Ctor, using the default penalty weights.
     *
     * @param element The {@code <eo>} element
     */
    Pretty(final Xnav element) {
        this(element, Pretty.DEFAULTS);
    }

    /**
     * Ctor.
     *
     * @param element The {@code <eo>} element
     * @param config The overridden weights; absent keys use their defaults
     */
    Pretty(final Xnav element, final Map<PenaltyKey, Integer> config) {
        this.root = element;
        this.style = new Style(config);
    }

    /**
     * Render the whole program as pretty EO source.
     *
     * @return EO source code
     */
    String asString() {
        final StringBuilder out = new StringBuilder(
            this.root.element("preamble").text().orElse("")
        );
        this.root.elements(Filter.withName("line"))
            .findFirst()
            .map(line -> new Node(line).print(this.style, 0))
            .ifPresent(out::append);
        return out.append('\n').toString();
    }
}
