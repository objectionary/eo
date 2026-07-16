/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

/**
 * A colored unified diff between two multi-line texts.
 *
 * <p>Both sides are split into lines and aligned by their longest
 * common subsequence. The result is rendered in the familiar unified
 * style: common lines are prefixed with a space, deletions with
 * {@code -} highlighted in red, and additions with {@code +} highlighted
 * in green. When the two texts are identical, {@link #colored()} returns
 * an empty string and {@link #same()} returns {@code true}.</p>
 *
 * @since 0.57.0
 */
final class Diff {

    /**
     * ANSI escape that resets all coloring.
     */
    private static final String RESET = "\u001b[0m";

    /**
     * ANSI escape that turns the text red (used for deletions).
     */
    private static final String RED = "\u001b[31m";

    /**
     * ANSI escape that turns the text green (used for additions).
     */
    private static final String GREEN = "\u001b[32m";

    /**
     * The text before the change.
     */
    private final String before;

    /**
     * The text after the change.
     */
    private final String after;

    /**
     * Ctor.
     * @param before The text before the change
     * @param after The text after the change
     */
    Diff(final String before, final String after) {
        this.before = before;
        this.after = after;
    }

    /**
     * Are the two texts identical?
     * @return TRUE if there is nothing to show
     */
    boolean same() {
        return this.before.equals(this.after);
    }

    /**
     * Render the difference as a colored unified diff.
     * @return The diff, or an empty string if the texts are identical
     */
    String colored() {
        final String result;
        if (this.same()) {
            result = "";
        } else {
            result = Diff.render(Diff.lines(this.before), Diff.lines(this.after));
        }
        return result;
    }

    /**
     * Split a text into lines.
     * @param text The text
     * @return The lines
     */
    private static List<String> lines(final String text) {
        return text.lines().collect(Collectors.toList());
    }

    /**
     * Render the unified diff of two already-split texts.
     * @param before The lines before the change
     * @param after The lines after the change
     * @return The colored unified diff
     */
    private static String render(final List<String> before, final List<String> after) {
        final int[][] lcs = Diff.lcs(before, after);
        final StringBuilder out = new StringBuilder(0);
        int row = 0;
        int col = 0;
        while (row < before.size() && col < after.size()) {
            if (before.get(row).equals(after.get(col))) {
                Diff.common(out, before.get(row));
                row += 1;
                col += 1;
            } else if (lcs[row + 1][col] >= lcs[row][col + 1]) {
                Diff.deleted(out, before.get(row));
                row += 1;
            } else {
                Diff.added(out, after.get(col));
                col += 1;
            }
        }
        Diff.drain(out, before.subList(row, before.size()), Diff::deleted);
        Diff.drain(out, after.subList(col, after.size()), Diff::added);
        return out.toString();
    }

    /**
     * Build the longest-common-subsequence length table.
     * @param before The lines before the change
     * @param after The lines after the change
     * @return The table, sized {@code (before + 1) x (after + 1)}
     */
    private static int[][] lcs(final List<String> before, final List<String> after) {
        final int rows = before.size();
        final int cols = after.size();
        final int[][] table = new int[rows + 1][cols + 1];
        for (int row = rows - 1; row >= 0; row -= 1) {
            for (int col = cols - 1; col >= 0; col -= 1) {
                if (before.get(row).equals(after.get(col))) {
                    table[row][col] = table[row + 1][col + 1] + 1;
                } else {
                    table[row][col] = Math.max(table[row + 1][col], table[row][col + 1]);
                }
            }
        }
        return table;
    }

    /**
     * Append the tail of one side that has no counterpart on the other.
     * @param out The output
     * @param tail The remaining lines of one side
     * @param appender How to append a single line (as deletion or addition)
     */
    private static void drain(
        final StringBuilder out, final List<String> tail,
        final BiConsumer<? super StringBuilder, ? super String> appender
    ) {
        for (final String line : tail) {
            appender.accept(out, line);
        }
    }

    /**
     * Append a common (unchanged) line.
     * @param out The output
     * @param line The line
     */
    private static void common(final StringBuilder out, final String line) {
        out.append(' ').append(line).append('\n');
    }

    /**
     * Append a deleted line, in red.
     * @param out The output
     * @param line The line
     */
    private static void deleted(final StringBuilder out, final String line) {
        out.append(Diff.RED).append('-').append(line).append(Diff.RESET).append('\n');
    }

    /**
     * Append an added line, in green.
     * @param out The output
     * @param line The line
     */
    private static void added(final StringBuilder out, final String line) {
        out.append(Diff.GREEN).append('+').append(line).append(Diff.RESET).append('\n');
    }
}
