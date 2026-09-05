/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Blank-line bookkeeping helpers for {@link Line} subclasses — §6.5 of
 * the spec.
 *
 * <p>R-6.5.7 caps consecutive blanks at one (enforced in
 * {@link LnBlank}). R-6.5.3 requires exactly one blank line in front of
 * every {@code +>} test attribute — enforced here by
 * {@link #checkTest}. R-6.5.4 forbids a blank line before a plain
 * child or between two plain siblings — enforced here by
 * {@link #checkPlain}.</p>
 *
 * <p>R-6.5.5 requires exactly one blank line between the meta header
 * and whatever follows; enforced by {@link #enterAfterMeta}, which
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
     * are exempt and call this method only when they want to
     * not exempt themselves.
     * @param span The offending line's span (used for error position)
     * @param globals The global parser state
     * @param emit The directives sink
     */
    static void checkPlain(final Span span, final Globals globals, final Emit emit) {
        Blanks.enterAfterMeta(span, globals, emit);
        if (
            globals.pendingBlanks() > 0
                && (globals.sealed() || globals.pendingComments().isEmpty())
        ) {
            emit.error(
                span.line(), span.indent(),
                "blank line not allowed between non-master siblings"
            );
        }
    }

    /**
     * Report a missing blank line in front of a {@code +>} test
     * attribute — illegal per R-6.5.3, which requires exactly one
     * blank line before every test attribute — and a test attribute
     * that sits deeper than a direct child of the top-level object,
     * illegal per R-6.3.3.
     *
     * <p>Indent 2 alone does not say "direct child of the top-level
     * object": it says so only when that object is a formation. When
     * the file's top-level object is an application — {@code bool >
     * true}, {@code number > nan}, {@code string > eol} — indent 2 is
     * an argument position, and a test attribute landing there would
     * silently become an argument named {@code Φ.+can-…} while its body
     * vanished from the XMIR. The outermost entry of {@code stack} is
     * therefore read as well, and anything but a formation is rejected
     * with the same error.</p>
     *
     * @param span The offending line's span (used for error position)
     * @param stack The indent stack, read for the top-level object's kind
     * @param globals The global parser state, read for the blank count
     * @param emit The directives sink
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void checkTest(
        final Span span, final Stack stack, final Globals globals, final Emit emit
    ) {
        final Kind top = stack.root().kind();
        if (span.indent() != 2 || top != Kind.BARE_FORMATION && top != Kind.ONLY_PHI) {
            emit.error(
                span.line(), span.indent(),
                "test attribute legal only as direct child of top-level object"
            );
        }
        if (globals.pendingBlanks() == 0) {
            emit.error(
                span.line(), span.indent(),
                "missing blank line before a `+>` test attribute (R-6.5.3); exactly one blank line must precede every test attribute"
            );
        }
    }

    /**
     * Close the meta header — called from the first non-meta non-blank
     * line, reporting R-6.5.5 when that line is not preceded by
     * exactly one blank line. Does nothing once the header is already
     * closed.
     * @param span The first post-meta line's span
     * @param globals The global parser state
     * @param emit The directives sink
     */
    static void enterAfterMeta(final Span span, final Globals globals, final Emit emit) {
        if (globals.inMetaHeader()) {
            if (globals.pendingBlanks() == 0) {
                emit.error(
                    span.line(), span.indent(),
                    "meta header must be followed by exactly one blank line"
                );
            } else {
                globals.clearBlanks();
            }
            globals.closeMetaHeader();
        }
    }
}
