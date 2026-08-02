/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Atom-signature validation shared by every non-formation line shape —
 * §3.10.10 of the spec.
 *
 * <p>An atom signature ({@code /sig}) declares that a formation's
 * behaviour is native code. Only {@link LnFormation} ever reads the
 * signature back out to emit the atom marker; every other line shape
 * that can carry a name suffix — plain application, method chain,
 * reversed dispatch, compact tuple, text block, and pipe application —
 * is no more a formation than a pipe is, so a {@code /sig} written on
 * one of them is the same user mistake {@link LnPipe} already rejects,
 * not a harmlessly-ignored no-op.</p>
 *
 * @since 0.1
 */
final class Atoms {

    /**
     * Utility class.
     */
    private Atoms() {
        // never called
    }

    /**
     * Reject a non-empty atom signature parsed on a line that is not a
     * formation.
     * @param suffix The parsed suffix
     * @param span The line's span (used for error position)
     */
    static void rejectOutsideFormation(final Suffix suffix, final Span span) {
        if (suffix.atom()) {
            throw new ParseError(
                span.line(), span.indent(),
                "only a formation can declare an atom signature"
            );
        }
    }
}
