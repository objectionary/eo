/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * One source line.
 *
 * <p>A {@code Span} is a value object carrying a single source line's text
 * (without trailing line terminator), its 1-indexed line number, and the
 * count of leading-space characters that precede the first non-space
 * character. The text never contains {@code \n} or {@code \r}; the
 * {@link Source} that produced this span has already normalised line
 * endings (R-2.1.2).</p>
 *
 * <p>The indent count is computed once, at construction time, and does not
 * mutate. A blank line (entire line is whitespace) yields a span with
 * {@code indent == text.length()}.</p>
 *
 * <p>Per spec R-2.2.1: an odd indent is a {@code unexpected odd indent}
 * error condition; {@code Span} itself does not raise it — the consumer
 * (line classifier) reads the indent and decides. The {@link #tab()} query
 * surfaces the R-2.2.4 violation; again, the consumer decides. *
 *
 * @since 0.1
 */
final class Span {

    /**
     * Line text without the trailing line terminator.
     */
    private final String text;

    /**
     * One-indexed source line number.
     */
    private final int number;

    /**
     * Count of leading space characters before the first non-space.
     */
    private final int indent;

    /**
     * Whether any leading whitespace character is a tab.
     */
    private final boolean tab;

    /**
     * Ctor.
     * @param body Line text
     * @param line Line number (1-indexed)
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    Span(final String body, final int line) {
        this(body, line, Span.leading(body), Span.tabbed(body));
    }

    /**
     * Primary ctor.
     * @param body Line text
     * @param line Line number
     * @param leading Count of leading space chars
     * @param tabbed True if any leading char is a tab
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    private Span(final String body, final int line, final int leading, final boolean tabbed) {
        this.text = body;
        this.number = line;
        this.indent = leading;
        this.tab = tabbed;
    }

    @Override
    public String toString() {
        return String.format(
            "Span(line=%d, indent=%d, text='%s')",
            this.number, this.indent, this.text
        );
    }

    /**
     * The full line text.
     * @return Text without terminator
     */
    String text() {
        return this.text;
    }

    /**
     * Source line number (1-indexed).
     * @return Line number
     */
    int line() {
        return this.number;
    }

    /**
     * Leading-space count.
     * @return Indent
     */
    int indent() {
        return this.indent;
    }

    /**
     * True if leading whitespace contains a tab character.
     * @return Tab flag
     */
    boolean tab() {
        return this.tab;
    }

    /**
     * True if the line is entirely whitespace.
     * @return Blank flag
     */
    boolean blank() {
        return this.indent == this.text.length();
    }

    /**
     * The substring after leading whitespace.
     * @return Tail text (empty for blank lines)
     */
    String body() {
        return this.text.substring(this.indent);
    }

    /**
     * The first non-space character, or {@code '\0'} for a blank line.
     * @return First non-space character
     */
    char head() {
        final char first;
        if (this.blank()) {
            first = '\0';
        } else {
            first = this.text.charAt(this.indent);
        }
        return first;
    }

    /**
     * Compute leading-space count.
     * @param body Line text
     * @return Number of leading {@code ' '} characters
     */
    private static int leading(final String body) {
        int count = 0;
        while (count < body.length() && body.charAt(count) == ' ') {
            count = count + 1;
        }
        return count;
    }

    /**
     * Detect a tab in the leading whitespace region.
     * @param body Line text
     * @return True if a tab character appears before the first non-whitespace
     */
    private static boolean tabbed(final String body) {
        boolean found = false;
        for (int idx = 0; idx < body.length(); idx = idx + 1) {
            final char glyph = body.charAt(idx);
            if (glyph == '\t') {
                found = true;
                break;
            }
            if (glyph != ' ') {
                break;
            }
        }
        return found;
    }
}
