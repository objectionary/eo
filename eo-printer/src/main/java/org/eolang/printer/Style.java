/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Map;

/**
 * The typographic style a {@link Node} lays itself out in.
 *
 * <p>It carries the two settings every rendering depends on: the width
 * of one indentation level and the weights a rendered block is scored
 * with. A {@link Node} renders itself against a style rather than
 * against the printer, so the dependency between the two keeps running
 * one way only and the layout of a node stays the node's own business.</p>
 *
 * @since 0.57.0
 */
final class Style {

    /**
     * The overridden penalty weights, by key.
     */
    private final Map<PenaltyKey, Integer> weights;

    /**
     * A single level of indentation, whose width is the {@code STEP} weight.
     */
    private final String tab;

    /**
     * Ctor.
     *
     * @param config The overridden weights; absent keys use their defaults
     */
    Style(final Map<PenaltyKey, Integer> config) {
        this(
            config,
            " ".repeat(
                config.getOrDefault(PenaltyKey.STEP, PenaltyKey.STEP.fallback())
            )
        );
    }

    /**
     * Ctor.
     *
     * @param config The overridden weights
     * @param step One level of indentation
     */
    private Style(final Map<PenaltyKey, Integer> config, final String step) {
        this.weights = config;
        this.tab = step;
    }

    /**
     * The blank prefix of a line sitting at the given indentation level.
     *
     * @param level The indentation level
     * @return The leading whitespace
     */
    String indent(final int level) {
        return this.tab.repeat(level);
    }

    /**
     * The penalty of a rendered block: the lower, the prettier.
     *
     * @param block The rendered block
     * @return The penalty points
     * @see Penalty
     */
    int points(final String block) {
        return new Penalty(block, this.weights).points();
    }
}
