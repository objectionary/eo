/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

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
 *   {@link #INDENT} points;</li>
 *   <li>every opening parenthesis costs {@link #BRACKET} points;</li>
 *   <li>every character sitting past the {@link #WIDTH}-th column
 *   costs {@link #OVERFLOW} point.</li>
 * </ul>
 *
 * <p>For example, this snippet has a penalty of 15 (five indents,
 * three points each):</p>
 *
 * <pre> [] &gt; foo
 *   gt. &gt; @
 *     42
 *     bar.hello 88</pre>
 *
 * <p>While this one, rendering the same object differently, scores
 * only 7 (a single opening parenthesis):</p>
 *
 * <pre> 42.gt (bar.hello 88) &gt; [] &gt; foo</pre>
 *
 * @since 0.57.0
 */
final class Penalty {

    /**
     * Points charged for each level of indentation on a line.
     */
    private static final int INDENT = 3;

    /**
     * Points charged for each opening parenthesis.
     */
    private static final int BRACKET = 7;

    /**
     * Points charged for each character past {@link #WIDTH}.
     */
    private static final int EXCESS = 1;

    /**
     * The column after which characters start being charged.
     */
    private static final int WIDTH = 80;

    /**
     * The width of a single indentation level, in spaces.
     */
    private static final int STEP = 2;

    /**
     * The EO source code to score.
     */
    private final String code;

    /**
     * Ctor.
     * @param source The EO source code
     */
    Penalty(final String source) {
        this.code = source;
    }

    /**
     * Calculate the total penalty.
     * @return The penalty in points
     */
    int points() {
        int total = 0;
        for (final String line : this.code.split("\n", -1)) {
            total += Penalty.indents(line) * Penalty.INDENT;
            total += Penalty.brackets(line) * Penalty.BRACKET;
            total += Penalty.overflow(line) * Penalty.EXCESS;
        }
        return total;
    }

    /**
     * Count indentation levels of a line.
     * @param line The line
     * @return The number of levels
     */
    private static int indents(final String line) {
        int spaces = 0;
        while (spaces < line.length() && line.charAt(spaces) == ' ') {
            ++spaces;
        }
        return spaces / Penalty.STEP;
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

    /**
     * Count characters past the allowed width.
     * @param line The line
     * @return The number of overflowing characters
     */
    private static int overflow(final String line) {
        return Math.max(0, line.length() - Penalty.WIDTH);
    }
}
