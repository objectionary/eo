/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A BYTES literal sitting at one place of a line body — §3.13 of the spec.
 *
 * <p>The literal is {@code --}, a single {@code BB-}, or {@code BB-BB(-BB)*}
 * with an optional continuation dash at its end (R-3.13.1). The two hex
 * digits of a byte are uppercase (R-3.13.1b), and the token owns every
 * character up to the first one that ends a token, so nothing of it may be
 * left for the reader that follows (R-3.13.1c).</p>
 *
 * <p>A token that opens with a hex digit and holds a dash is a literal
 * either way, well formed or not, which is what lets a malformed one be
 * named at the column it starts on rather than at the character the
 * reader stopped on.</p>
 *
 * @since 0.1
 */
final class Bytes {

    /**
     * The line body the literal sits in.
     */
    private final String text;

    /**
     * The index the literal starts at.
     */
    private final int from;

    /**
     * Ctor.
     *
     * @param body Line body
     * @param start Index of the first character of the literal
     */
    Bytes(final String body, final int start) {
        this.text = body;
        this.from = start;
    }

    /**
     * Whether a BYTES literal starts here, well formed or not.
     *
     * @return Opening flag
     */
    boolean opens() {
        return this.empty() || this.chunk() || this.attempt();
    }

    /**
     * Whether a run of hex digits followed by a dash starts here. The
     * run is too short for a byte pair, so it opens no literal, yet it
     * is no identifier either: {@code A-} is a malformed BYTES.
     *
     * @return Odd-run flag
     */
    boolean odd() {
        int idx = this.from;
        while (idx < this.text.length() && Bytes.digit(this.text.charAt(idx))) {
            idx = idx + 1;
        }
        return idx > this.from
            && idx < this.text.length()
            && this.text.charAt(idx) == '-';
    }

    /**
     * The index just past the literal, which is also its end.
     *
     * @param span The line, for the position of an error
     * @return Index past the last character of the literal
     */
    int end(final Span span) {
        final int end;
        if (this.empty()) {
            end = this.from + 2;
        } else {
            end = this.pairs(span);
        }
        return end;
    }

    private int pairs(final Span span) {
        if (!this.pair(this.from)) {
            throw this.malformed(span);
        }
        int idx = this.from + 2;
        while (idx < this.text.length()
            && this.text.charAt(idx) == '-'
            && this.pair(idx + 1)) {
            idx = idx + 3;
        }
        if (idx < this.text.length() && this.text.charAt(idx) == '-') {
            if (idx - this.from > 2 && this.closes(idx + 1)) {
                throw new ParseError(
                    span.line(), span.indent() + idx,
                    "bytes literal ends with a dangling continuation dash"
                );
            }
            idx = idx + 1;
        }
        if (!this.closes(idx)) {
            throw this.malformed(span);
        }
        return idx;
    }

    private ParseError malformed(final Span span) {
        return new ParseError(
            span.line(), span.indent() + this.from, "invalid bytes literal"
        );
    }

    private boolean empty() {
        return this.from + 1 < this.text.length()
            && this.text.charAt(this.from) == '-'
            && this.text.charAt(this.from + 1) == '-';
    }

    private boolean chunk() {
        return this.from + 2 < this.text.length()
            && this.pair(this.from)
            && this.text.charAt(this.from + 2) == '-';
    }

    private boolean attempt() {
        final String token = this.token();
        return !token.isEmpty()
            && Bytes.digit(token.charAt(0))
            && token.indexOf('-') >= 0
            && token.chars().allMatch(Bytes::shaped);
    }

    private String token() {
        int end = this.from;
        while (!this.closes(end)) {
            end = end + 1;
        }
        return this.text.substring(this.from, end);
    }

    private boolean closes(final int idx) {
        return idx >= this.text.length() || Tokens.terminates(this.text.charAt(idx));
    }

    private boolean pair(final int idx) {
        return idx + 1 < this.text.length()
            && Bytes.digit(this.text.charAt(idx))
            && Bytes.digit(this.text.charAt(idx + 1));
    }

    private static boolean shaped(final int glyph) {
        return glyph == '-' || glyph < 128 && Character.isLetterOrDigit(glyph);
    }

    private static boolean digit(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }
}
