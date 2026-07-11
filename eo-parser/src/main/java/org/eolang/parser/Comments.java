/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * Flush the top comment block and close the header zone — §3.3 / §6.4.
 *
 * <p>Only one comment block is legal in a program: the block that sits
 * on top of the file, before all metas and objects (§3.3). It documents
 * the whole program. As each {@link LnComment} arrives it accumulates in
 * {@link Globals#pendingComments()}; the first meta directive or object
 * then calls {@link #seal(Globals, Emit, Span)} to flush that block into
 * {@code /object/comments/comment} and mark the header zone closed. From
 * that point on any further comment is rejected by {@link LnComment}.</p>
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
     * Flush the pending top comment block, if any, and close the header
     * zone.
     *
     * <p>Idempotent — after the first call {@link Globals#sealed()} is
     * true and subsequent calls do nothing, so only the first meta or
     * object in the file triggers the flush.</p>
     *
     * <p>A non-empty top comment block must be separated from the rest
     * of the file by exactly one blank line (§6.5). If the sealing meta
     * or object follows the block with no blank in between
     * ({@link Globals#pendingBlanks()} is zero), the block is rejected.</p>
     *
     * @param globals Global parser state
     * @param emit XMIR emitter
     * @param span Source span of the meta or object closing the header
     */
    static void seal(final Globals globals, final Emit emit, final Span span) {
        if (globals.sealed()) {
            return;
        }
        final List<Span> pending = globals.pendingComments();
        if (!pending.isEmpty() && globals.pendingBlanks() == 0) {
            throw new ParseError(
                span.line(), span.indent(),
                "a blank line must separate the top comment block from the rest of the file"
            );
        }
        if (!pending.isEmpty()) {
            emit.comment(pending, span.line());
            globals.clearComments();
        }
        globals.seal();
    }
}
