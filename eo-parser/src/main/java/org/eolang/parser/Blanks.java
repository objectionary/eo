/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Blank-line bookkeeping helpers for {@link Line} subclasses — §6.5 of
 * the spec.
 *
 * <p>R-6.5.3 caps consecutive blanks at one (enforced in
 * {@link LnBlank}) and requires exactly one blank line in front of
 * every {@code +>} test attribute — enforced here by
 * {@link #checkTest}. R-6.5.4 forbids a blank line before a plain
 * child or between two plain siblings — enforced here by
 * {@link #checkPlain}.</p>
 *
 * <p>R-6.5.5 requires exactly one blank line between the meta header
 * and whatever follows; enforced by {@link #checkAfterMetas}, which
 * fires from the first non-meta non-blank line when the parser has
 * accumulated meta directives but not yet seen any blank.</p>
 *
 * @since 0.1
 */
final class Blanks {

    /**
     * Utility class.
     */
    private Blanks() {
        // never called
    }

    /**
     * Report a blank line in front of a plain child or between two
     * plain siblings — illegal per R-6.5.4. Master children
     * (formations, atoms, only-phi formations, {@code +>} tests)
     * are exempt and call this method only when they want to *not*
     * exempt themselves.
     * @param span The offending line's span (used for error position)
     * @param globals The global parser state
     * @param emit The directives sink
     */
    static void checkPlain(final Span span, final Globals globals, final Emit emit) {
        Blanks.enterAfterMeta(span, globals, emit);
        if (globals.pendingBlanks() > 0) {
            emit.error(
                span.line(), span.indent(),
                "blank line before a plain object is forbidden (R-6.5.4); only master objects (formations, atoms, only-phi formations, +> tests) may be preceded by a blank line"
            );
        }
    }

    /**
     * Report a missing blank line in front of a {@code +>} test
     * attribute — illegal per R-6.5.3, which requires exactly one
     * blank line before every test attribute.
     * @param span The offending line's span (used for error position)
     * @param globals The global parser state
     * @param emit The directives sink
     */
    static void checkTest(final Span span, final Globals globals, final Emit emit) {
        if (globals.pendingBlanks() == 0) {
            emit.error(
                span.line(), span.indent(),
                "missing blank line before a `+>` test attribute (R-6.5.3); exactly one blank line must precede every test attribute"
            );
        }
    }

    /**
     * Report R-6.5.5 — the first non-meta non-blank line after the
     * meta header must be preceded by exactly one blank line. Closes
     * the meta-header window so subsequent lines are not re-checked.
     * @param span The first post-meta line's span
     * @param globals The global parser state
     * @param emit The directives sink
     */
    static void enterAfterMeta(final Span span, final Globals globals, final Emit emit) {
        if (globals.inMetaHeader()) {
            if (globals.pendingBlanks() == 0) {
                emit.error(
                    span.line(), span.indent(),
                    "missing blank line between meta header and the first non-meta line (R-6.5.5); exactly one blank must separate them"
                );
            } else {
                globals.clearBlanks();
            }
            globals.closeMetaHeader();
        }
    }
}
