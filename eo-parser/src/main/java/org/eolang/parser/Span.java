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
 * count of leading-whitespace characters that precede the first
 * non-whitespace character. The text never contains {@code \n} or {@code \r}; the
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
 * surfaces the R-2.2.4 violation; again, the consumer decides.</p>
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
     * Count of leading whitespace characters before the first
     * non-whitespace one.
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
     */
    Span(final String body, final int line) {
        this(body, line, Span.leading(body));
    }

    /**
     * Ctor.
     * @param body Line text
     * @param line Line number
     * @param leading Count of leading whitespace chars
     */
    private Span(final String body, final int line, final int leading) {
        this(body, line, leading, Span.tabbed(body, leading));
    }

    /**
     * Primary ctor.
     * @param body Line text
     * @param line Line number
     * @param leading Count of leading whitespace chars
     * @param tabbed True if any leading char is a tab
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
     * Leading-whitespace count.
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
     * True if the leading whitespace holds a character that is neither a
     * space nor a tab. An indent is made of spaces (R-2.2.1), and a
     * character nobody can see in an editor must not decide how deep a
     * line sits: a pair of form feeds reads as indent 1 to a counter that
     * takes every whitespace character (#7924).
     * @return Alien-whitespace flag
     */
    boolean alien() {
        boolean found = false;
        for (int idx = 0; idx < this.indent; idx = idx + 1) {
            final char glyph = this.text.charAt(idx);
            if (glyph != ' ' && glyph != '\t') {
                found = true;
                break;
            }
        }
        return found;
    }

    /**
     * True if the line is entirely whitespace.
     * @return Blank flag
     */
    boolean blank() {
        return this.indent == this.text.length();
    }

    /**
     * True if the line is not blank and its last character is a space or a
     * tab. Whitespace nobody can see in an editor must not decide what a
     * program means (R-2.2.5).
     * @return Trailing-whitespace flag
     */
    boolean trailing() {
        final boolean result;
        if (this.blank()) {
            result = false;
        } else {
            final char last = this.text.charAt(this.text.length() - 1);
            result = last == ' ' || last == '\t';
        }
        return result;
    }

    /**
     * The substring after leading whitespace.
     * @return Tail text (empty for blank lines)
     */
    String body() {
        return this.text.substring(this.indent);
    }

    /**
     * The first non-whitespace character.
     * @return First non-whitespace character
     */
    char head() {
        if (this.blank()) {
            throw new IllegalStateException(
                String.format(
                    "line %d is blank, has no first non-whitespace character",
                    this.number
                )
            );
        }
        return this.text.charAt(this.indent);
    }

    private static int leading(final String body) {
        int count = 0;
        while (count < body.length() && Character.isWhitespace(body.charAt(count))) {
            count = count + 1;
        }
        return count;
    }

    private static boolean tabbed(final String body, final int leading) {
        boolean found = false;
        for (int idx = 0; idx < leading; idx = idx + 1) {
            if (body.charAt(idx) == '\t') {
                found = true;
                break;
            }
        }
        return found;
    }
}
