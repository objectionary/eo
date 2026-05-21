/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * A comment line — §3.3 of the spec.
 *
 * <p>A line whose first non-space character is {@code #}. Comment lines
 * accumulate into a <em>comment block</em> attached to the next named
 * object at the same indent (R-3.3.2 / §6.4). Until that named object
 * appears, this line buffers its span in {@link Globals#addComment(Span)};
 * the walker (or the next named line) flushes the buffer when it
 * attaches.</p>
 *
 * <p>Cross-line rules enforced here:</p>
 *
 * <ul>
 *   <li>R-3.3.3 — every comment in a block shares one indent. When a new
 *   comment arrives whose indent differs from the head of the buffer,
 *   the existing buffer is reported as dangling (it cannot attach
 *   forwards) and the new comment starts a fresh block.</li>
 *   <li>R-3.3.4 / R-6.5.1 — no blank line inside a comment block. When
 *   the new comment arrives with {@code pendingBlanks() > 0} <em>and</em>
 *   the buffer is non-empty, the prior block is dangling — a blank line
 *   between two comment lines splits them into two blocks per R-3.3.7.</li>
 * </ul>
 *
 * @since 0.1
 */
final class LnComment implements Line {

    /**
     * The comment line's span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The comment span
     */
    LnComment(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        Blanks.enterAfterMeta(this.span, globals, emit);
        final List<Span> pending = globals.pendingComments();
        if (!pending.isEmpty()) {
            final Span head = pending.get(0);
            if (head.indent() != this.span.indent() || globals.pendingBlanks() > 0) {
                emit.error(
                    head.line(), head.indent(),
                    "comment must precede a named object"
                );
                globals.clearComments();
            }
        }
        globals.addComment(this.span);
        globals.clearBlanks();
    }
}
