/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.regex.Pattern;
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
 * <p>Line terminators are part of what is compared, since two texts that
 * differ only in them are not the same text and a diff that shows nothing
 * changed would say the opposite. A carriage return is rendered as
 * {@code \r}, and a text that does not end with a newline carries the
 * note {@code git} prints for it.</p>
 *
 * @since 0.57.0
 */
final class Diff {

    /**
     * The line feed every line ends with.
     */
    private static final Pattern FEED = Pattern.compile("\\n");

    /**
     * The carriage return a line may end with.
     */
    private static final Pattern RETURN = Pattern.compile("\\r");

    /**
     * ANSI escape that resets all coloring.
     */
    private static final String RESET = "\033[0m";

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

    private static List<String> lines(final String text) {
        final List<String> out = new ArrayList<>(
            Arrays.stream(Diff.FEED.split(text, -1))
                .map(line -> Diff.RETURN.matcher(line).replaceAll("\\\\r"))
                .collect(Collectors.toList())
        );
        if (out.get(out.size() - 1).isEmpty()) {
            out.remove(out.size() - 1);
        } else {
            out.add("\\ No newline at end of file");
        }
        return out;
    }

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

    private static void drain(
        final StringBuilder out, final List<String> tail,
        final BiConsumer<? super StringBuilder, ? super String> appender
    ) {
        for (final String line : tail) {
            appender.accept(out, line);
        }
    }

    private static void common(final StringBuilder out, final String line) {
        out.append(' ').append(line).append('\n');
    }

    private static void deleted(final StringBuilder out, final String line) {
        out.append("\033[31m-").append(line).append(Diff.RESET).append('\n');
    }

    private static void added(final StringBuilder out, final String line) {
        out.append("\033[32m+").append(line).append(Diff.RESET).append('\n');
    }
}
