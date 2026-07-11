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
 * as each {@link LnComment} arrives. Every subsequent line — named or
 * not — calls {@link #attach(Globals, Emit, Span, boolean)}: a blank
 * line separating the block from what follows is an immediate
 * R-6.5.2 violation, reported as dangling per R-6.4.2 and cleared on
 * the spot. Absent a blank, the block flushes into
 * {@code /object/comments/comment} as soon as a named line at the
 * same indent arrives; an unnamed or deeper/shallower-indent line in
 * between is not itself a violation — the block stays pending so a
 * later same-indent named line (e.g. the name-bearing tail of a
 * vertical-continuation chain) can still claim it. Any block left
 * pending at end of stream is reported dangling by the EOF check in
 * {@link Eo}.</p>
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
        if (globals.pendingBlanks() > 0) {
            emit.error(
                head.line(), head.indent(),
                "comment must precede a named object"
            );
            globals.clearComments();
        } else if (named && head.indent() == span.indent()) {
            emit.comment(pending, span.line());
            globals.clearComments();
        }
    }
}
