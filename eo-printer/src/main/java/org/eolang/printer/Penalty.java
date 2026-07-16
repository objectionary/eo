/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Collections;
import java.util.Map;

/**
 * Penalty of a piece of EO source code.
 *
 * <p>The penalty is a numeric estimate of how "ugly" a fragment of EO
 * looks. The lower the penalty, the prettier the code. It is used by
 * {@link Pretty} to choose, among all possible renderings of the same
 * object, the one that reads best.</p>
 *
 * <p>The score is a sum of three components, computed over all lines:</p>
 *
 * <ul>
 *   <li>every level of indentation on a line costs
 *   {@link PenaltyKey#INDENT} points;</li>
 *   <li>opening parentheses on a line cost {@link PenaltyKey#BRACKET}
 *   points progressively — the k-th costs k times the weight, so n on
 *   one line cost n(n+1)/2 times the weight;</li>
 *   <li>every character sitting past the {@link PenaltyKey#WIDTH}-th
 *   column costs {@link PenaltyKey#EXCESS} point;</li>
 *   <li>every symbol in the block costs {@link PenaltyKey#SYMBOL}
 *   point.</li>
 * </ul>
 *
 * <p>All of these weights, together with the indentation
 * {@link PenaltyKey#STEP}, are tunable: they are read from a
 * {@code Map<PenaltyKey, Integer>} supplied to the constructor, and any
 * key absent from that map falls back to its {@link PenaltyKey#fallback()}
 * default.</p>
 *
 * <p>For example, with the default weights this snippet has a penalty of
 * 64 (five indents at five points each, plus 39 symbols):</p>
 *
 * <pre> [] &gt; foo
 *   gt. &gt; @
 *     42
 *     bar.hello 88</pre>
 *
 * <p>While this one, rendering the same object differently, scores
 * only 46 (one opening parenthesis at fifteen points, plus 31
 * symbols):</p>
 *
 * <pre> 42.gt (bar.hello 88) &gt; [] &gt; foo</pre>
 *
 * @since 0.57.0
 */
final class Penalty {

    /**
     * The default weights: an empty map, so every key uses its fallback.
     */
    private static final Map<PenaltyKey, Integer> DEFAULTS = Collections.emptyMap();

    /**
     * The EO source code to score.
     */
    private final String code;

    /**
     * The overridden weights, by key.
     */
    private final Map<PenaltyKey, Integer> weights;

    /**
     * Ctor, using the default weights for every key.
     * @param source The EO source code
     */
    Penalty(final String source) {
        this(source, Penalty.DEFAULTS);
    }

    /**
     * Ctor.
     * @param source The EO source code
     * @param config The overridden weights; absent keys use their defaults
     */
    Penalty(final String source, final Map<PenaltyKey, Integer> config) {
        this.code = source;
        this.weights = config;
    }

    /**
     * Calculate the total penalty.
     * @return The penalty in points
     */
    int points() {
        int total = 0;
        for (final String line : this.code.split(String.valueOf('\n'), -1)) {
            final int opened = Penalty.brackets(line);
            total += this.indents(line) * this.weight(PenaltyKey.INDENT);
            total += opened * (opened + 1) / 2 * this.weight(PenaltyKey.BRACKET);
            total += this.overflow(line) * this.weight(PenaltyKey.EXCESS);
            total += line.length() * this.weight(PenaltyKey.SYMBOL);
        }
        return total;
    }

    /**
     * The weight of a key, or its default if not overridden.
     * @param key The key
     * @return The weight in points
     */
    private int weight(final PenaltyKey key) {
        return this.weights.getOrDefault(key, key.fallback());
    }

    /**
     * Count indentation levels of a line.
     * @param line The line
     * @return The number of levels
     */
    private int indents(final String line) {
        int spaces = 0;
        while (spaces < line.length() && line.charAt(spaces) == ' ') {
            ++spaces;
        }
        return spaces / this.weight(PenaltyKey.STEP);
    }

    /**
     * Count characters past the allowed width.
     * @param line The line
     * @return The number of overflowing characters
     */
    private int overflow(final String line) {
        return Math.max(0, line.length() - this.weight(PenaltyKey.WIDTH));
    }

    /**
     * Count opening parentheses in a line.
     * @param line The line
     * @return The number of parentheses
     */
    private static int brackets(final String line) {
        int count = 0;
        for (int idx = 0; idx < line.length(); ++idx) {
            if (line.charAt(idx) == '(') {
                ++count;
            }
        }
        return count;
    }
}
