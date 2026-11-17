/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * A colored unified diff between two multi-line texts.
 *
 * <p>Both sides are split into lines and aligned by their longest
 * common subsequence. The result is rendered in the familiar unified
 * style: deletions are prefixed with {@code -} highlighted in red,
 * additions with {@code +} highlighted in green, and each change is shown
 * with a few unchanged lines around it, under an {@code @@} header that
 * says where in either text the change sits. A line further away than
 * that is not printed at all, so one wrong line in a long file prints
 * nine lines and not the whole file. When the two texts are identical,
 * {@link #colored()} returns an empty string and {@link #same()} returns
 * {@code true}.</p>
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
     * How many unchanged lines are shown on either side of a change.
     */
    private static final int CONTEXT = 3;

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
     *
     * @param before The text before the change
     * @param after The text after the change
     */
    Diff(final String before, final String after) {
        this.before = before;
        this.after = after;
    }

    /**
     * Are the two texts identical?
     *
     * @return TRUE if there is nothing to show
     */
    boolean same() {
        return this.before.equals(this.after);
    }

    /**
     * Render the difference as a colored unified diff.
     *
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
        final List<String> script = Diff.script(before, after);
        final StringBuilder out = new StringBuilder(0);
        int idx = 0;
        while (idx < script.size()) {
            if (' ' == script.get(idx).charAt(0)) {
                idx += 1;
            } else {
                final int last = Diff.closing(script, idx);
                final int till = Math.min(script.size(), last + 1 + Diff.CONTEXT);
                Diff.hunk(out, script, Math.max(0, idx - Diff.CONTEXT), till);
                idx = till;
            }
        }
        return out.toString();
    }

    private static List<String> script(final List<String> before, final List<String> after) {
        final int[][] lcs = Diff.lcs(before, after);
        final List<String> out = new ArrayList<>(0);
        int row = 0;
        int col = 0;
        while (row < before.size() && col < after.size()) {
            if (before.get(row).equals(after.get(col))) {
                out.add(String.join("", " ", before.get(row)));
                row += 1;
                col += 1;
            } else if (lcs[row + 1][col] >= lcs[row][col + 1]) {
                out.add(String.join("", "-", before.get(row)));
                row += 1;
            } else {
                out.add(String.join("", "+", after.get(col)));
                col += 1;
            }
        }
        Diff.drain(out, before.subList(row, before.size()), "-");
        Diff.drain(out, after.subList(col, after.size()), "+");
        return out;
    }

    private static int closing(final List<String> script, final int start) {
        int last = start;
        int idx = start;
        while (idx < script.size() && idx - last <= 2 * Diff.CONTEXT) {
            if (' ' != script.get(idx).charAt(0)) {
                last = idx;
            }
            idx += 1;
        }
        return last;
    }

    private static void hunk(
        final StringBuilder out, final List<String> script, final int from, final int till
    ) {
        out.append(
            String.format(
                "@@ -%d,%d +%d,%d @@",
                Diff.counted(script, 0, from, '+') + 1,
                Diff.counted(script, from, till, '+'),
                Diff.counted(script, 0, from, '-') + 1,
                Diff.counted(script, from, till, '-')
            )
        ).append('\n');
        for (final String line : script.subList(from, till)) {
            if ('-' == line.charAt(0)) {
                Diff.deleted(out, line.substring(1));
            } else if ('+' == line.charAt(0)) {
                Diff.added(out, line.substring(1));
            } else {
                Diff.common(out, line.substring(1));
            }
        }
    }

    private static int counted(
        final List<String> script, final int from, final int till, final char skip
    ) {
        int count = 0;
        for (final String line : script.subList(from, till)) {
            if (skip != line.charAt(0)) {
                count += 1;
            }
        }
        return count;
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
        final List<String> out, final List<String> tail, final String mark
    ) {
        for (final String line : tail) {
            out.add(String.join("", mark, line));
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
