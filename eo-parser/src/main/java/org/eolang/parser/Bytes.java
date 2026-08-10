/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * BYTES literal shape helpers.
 * @since 0.1
 */
final class Bytes {

    /**
     * Ctor.
     */
    private Bytes() {
    }

    /**
     * Whether a span body starts a multi-line BYTES literal.
     * @param body The line body
     * @return True if a BYTES continuation starts here
     */
    static boolean continuation(final String body) {
        return body.length() >= 6 && body.endsWith("-") && Bytes.only(body);
    }

    /**
     * Merge continuation lines that belong to the BYTES head.
     * @param spans Materialised source spans
     * @param start Index of the continuation start
     * @return Merged continuation
     */
    static Continuation merge(final List<Span> spans, final int start) {
        final Span head = spans.get(start);
        final StringBuilder body = new StringBuilder(head.body());
        int idx = start + 1;
        while (Bytes.next(spans, head, body, idx)) {
            final String line = spans.get(idx).body();
            body.append(line);
            idx = idx + 1;
            if (!line.endsWith("-")) {
                break;
            }
        }
        return new Bytes.Continuation(
            body.toString(), idx,
            body.toString().endsWith("-")
                && (idx >= spans.size() || spans.get(idx).blank())
        );
    }

    /**
     * Whether a byte pair starts at the index.
     * @param body The line body
     * @param idx The index
     * @return True if two BYTES hex digits start at the index
     */
    static boolean pair(final String body, final int idx) {
        return idx + 1 < body.length()
            && Bytes.hex(body.charAt(idx))
            && Bytes.hex(body.charAt(idx + 1));
    }

    /**
     * Whether the next span belongs to the merged BYTES body.
     * @param spans Source spans
     * @param head First span of the BYTES continuation
     * @param body Current merged body
     * @param idx Candidate span index
     * @return True if candidate should be merged
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    private static boolean next(
        final List<Span> spans, final Span head, final StringBuilder body,
        final int idx
    ) {
        final boolean found;
        if (idx >= spans.size() || spans.get(idx).indent() < head.indent()) {
            found = false;
        } else if (Bytes.only(spans.get(idx).body())) {
            found = true;
        } else {
            found = body.toString().endsWith("-")
                && Bytes.head(spans.get(idx).body());
        }
        return found;
    }

    /**
     * Whether the body is purely a sequence of hex bytes.
     * @param body The line body
     * @return True if the body matches the bytes-only pattern
     */
    private static boolean only(final String body) {
        boolean valid = !body.isEmpty();
        int idx = 0;
        while (valid && idx < body.length()) {
            if (Bytes.pair(body, idx)) {
                idx = idx + 2;
                if (idx < body.length() && body.charAt(idx) != '-') {
                    valid = false;
                } else {
                    idx = idx + 1;
                }
            } else {
                valid = false;
            }
        }
        return valid;
    }

    /**
     * Whether the body starts with a BYTES chunk followed by a suffix.
     * @param body The line body
     * @return True if a bytes chunk starts the line
     */
    private static boolean head(final String body) {
        int idx = 0;
        boolean valid = Bytes.pair(body, idx);
        if (valid) {
            idx = idx + 2;
            while (idx < body.length() && body.charAt(idx) == '-'
                && Bytes.pair(body, idx + 1)) {
                idx = idx + 3;
            }
            valid = idx < body.length() && Character.isWhitespace(body.charAt(idx));
        }
        return valid;
    }

    /**
     * Whether a character is a valid BYTES hex digit.
     * @param glyph The character
     * @return True if 0-9 or A-F
     */
    private static boolean hex(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }

    /**
     * Merged BYTES continuation.
     * @since 0.1
     */
    static final class Continuation {

        /**
         * Body to process.
         */
        private final String text;

        /**
         * Next span index.
         */
        private final int index;

        /**
         * Whether the continuation is unterminated.
         */
        private final boolean dangling;

        /**
         * Ctor.
         * @param body Body to process
         * @param next Next span index
         * @param unterminated Whether the continuation is unterminated
         */
        Continuation(
            final String body, final int next, final boolean unterminated
        ) {
            this.text = body;
            this.index = next;
            this.dangling = unterminated;
        }

        /**
         * Body to process.
         * @return Body
         */
        String body() {
            return this.text;
        }

        /**
         * Next span index.
         * @return Index
         */
        int next() {
            return this.index;
        }

        /**
         * Whether the continuation is unterminated.
         * @return True when no following chunk terminates the continuation
         */
        boolean unterminated() {
            return this.dangling;
        }
    }
}
