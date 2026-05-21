/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * Attach buffered comment lines to the next named object — §6.4.
 *
 * <p>A comment block accumulates in {@link Globals#pendingComments()}
 * as each {@link LnComment} arrives. The next named line — any
 * formation, application, method or reversed line whose suffix carries
 * a name — calls {@link #attach(Globals, Emit, Span, boolean)} to
 * either flush the block into {@code /object/comments/comment} (when
 * the line is named, sits at the same indent, and has no intervening
 * blank line — R-6.5.2) or report the block as dangling per
 * R-6.4.2.</p>
 *
 * <p>The buffer is always cleared after the call so a dangling block
 * is not re-reported by the EOF check.</p>
 *
 * @since 0.1
 */
final class Comments {

    /**
     * No instances.
     */
    private Comments() {
    }

    /**
     * Flush or reject the pending comment block in light of the line
     * about to be emitted.
     * @param globals Global parser state
     * @param emit XMIR emitter
     * @param span Source span of the upcoming line
     * @param named True when the upcoming line carries a name suffix
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void attach(
        final Globals globals, final Emit emit, final Span span, final boolean named
    ) {
        final List<Span> pending = globals.pendingComments();
        if (pending.isEmpty()) {
            return;
        }
        final Span head = pending.get(0);
        if (named && head.indent() == span.indent() && globals.pendingBlanks() == 0) {
            emit.comment(pending, span.line());
            globals.clearComments();
        }
    }
}
