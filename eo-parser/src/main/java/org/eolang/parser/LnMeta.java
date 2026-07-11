/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * A meta-directive line — §3.2 of the spec.
 *
 * <p>Form: {@code +name [parts]}. The name follows the leading {@code +}
 * with no space; parts are space-separated tokens that follow. Legal only
 * at indent 0 (R-3.2.1) and only before any non-meta object has been
 * emitted (R-3.2.2). At most one space between parts (R-3.2.4).</p>
 *
 * <p>A leading {@code Q} in any part is promoted to {@code Φ} in the
 * emitted XMIR (R-3.2.3 / R-9.3). This class does the promotion at
 * emission time. *
 *
 * @since 0.1
 */
final class LnMeta implements Line {

    /**
     * The meta line's span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The meta span
     */
    LnMeta(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        if (this.span.indent() != 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "meta directive must precede all other objects"
            );
        }
        if (globals.firstObjectEmitted()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "meta directive must precede all other objects"
            );
        }
        if (globals.inMetaHeader() && globals.pendingBlanks() > 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "blank line between meta directives is forbidden (R-6.5.5); the meta header is a single contiguous block"
            );
        }
        final String body = this.span.body();
        final int space = body.indexOf(' ');
        final String head;
        final List<String> parts;
        if (space < 0) {
            head = body.substring(1);
            parts = new ArrayList<>(0);
        } else {
            head = body.substring(1, space);
            parts = LnMeta.split(
                body.substring(space + 1), this.span, space + 1
            );
        }
        Comments.seal(globals, emit, this.span);
        globals.markMeta();
        globals.clearBlanks();
        emit.meta(this.span.line(), head, parts);
    }

    /**
     * Split the parts tail by single space, rejecting double spaces per
     * R-3.2.4 and promoting any bare {@code Q} to {@code Φ} per
     * R-3.2.3 / R-9.3.
     * @param tail Substring after the {@code +name} prefix
     * @param span Source span (for error reporting)
     * @param base Body-relative offset where the tail starts
     * @return Parts in source order
     */
    private static List<String> split(
        final String tail, final Span span, final int base
    ) {
        final List<String> out = new ArrayList<>(2);
        int idx = 0;
        while (idx < tail.length()) {
            if (tail.charAt(idx) == ' ') {
                throw new ParseError(
                    span.line(), span.indent() + base + idx,
                    "meta parts must be separated by exactly one space"
                );
            }
            int end = idx;
            while (end < tail.length() && tail.charAt(end) != ' ') {
                end = end + 1;
            }
            out.add(LnMeta.promoteQ(tail.substring(idx, end)));
            idx = end;
            if (idx < tail.length()) {
                idx = idx + 1;
            }
        }
        return out;
    }

    /**
     * Promote a leading {@code Q} in a part to {@code Φ}.
     * @param part The part text
     * @return Part with {@code Q} → {@code Φ} promotion if applicable
     */
    private static String promoteQ(final String part) {
        final String promoted;
        if (part.equals("Q")) {
            promoted = "Φ";
        } else if (part.startsWith("Q.")) {
            promoted = "Φ".concat(part.substring(1));
        } else {
            promoted = part;
        }
        return promoted;
    }
}
