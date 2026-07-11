/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A comment line — §3.3 of the spec.
 *
 * <p>A line whose first non-space character is {@code #}. Only one
 * comment block is legal in a program: a contiguous run of {@code #}
 * lines at indent zero, on top of the file, before all metas and
 * objects (R-3.3.2). It documents the whole program and accumulates in
 * {@link Globals#addComment(Span)} until the first meta or object flushes
 * it (§6.4). Every other comment — indented, after a meta, or after an
 * object — is rejected (R-3.3.6).</p>
 *
 * <p>Cross-line rules enforced here:</p>
 *
 * <ul>
 *   <li>R-3.3.6 — a comment is allowed only in the top block. Once the
 *   header zone is sealed ({@link Globals#sealed()}), or the comment
 *   is indented, it is rejected.</li>
 *   <li>R-3.3.4 / R-6.5.1 — no blank line inside the comment block. A
 *   blank arriving mid-block (buffer non-empty and
 *   {@code pendingBlanks() > 0}) breaks it apart and is rejected.</li>
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
        if (globals.sealed() || this.span.indent() != 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "comment is allowed only on top of the file, before metas"
            );
        }
        if (!globals.pendingComments().isEmpty() && globals.pendingBlanks() > 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "blank line inside the top comment block is not allowed"
            );
        }
        globals.addComment(this.span);
        globals.clearBlanks();
    }
}
