/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A classified source line — §3 of the spec.
 *
 * <p>One implementation per row of the §3.1 classification table. Each
 * impl owns its line shape: the per-line rules from §3.x and the
 * cross-line transition from §5.2 fire inside its {@link #into}. The
 * separation by line shape mirrors the spec's own taxonomy — a reader
 * tracing rule {@code R-3.K} reads the corresponding {@code *Line}
 * class.</p>
 *
 * <p>The contract is intentionally single-method: classification produces
 * one {@code Line} per {@link Span}, and the walker drives the parser by
 * iterating over those lines and invoking {@code into} for each. State
 * mutation is confined to the supplied {@link Stack} and {@link Globals};
 * emission to the supplied {@link Emit}.</p>
 *
 * @since 0.1
 */
@FunctionalInterface
interface Line {

    /**
     * Apply this line to the parser state.
     *
     * <p>The impl reads the current top of {@code stack}, runs any
     * applicable §5.2 transition (push/replace/pop), updates global
     * flags in {@code globals}, and writes XMIR directives to
     * {@code emit}. Implementations should fail fast: throw a
     * {@link ParseError} the moment a per-line or cross-line rule is
     * violated; the walker takes a savepoint before calling {@code into}
     * and rolls back on the throw (R-7.2).</p>
     *
     * @param stack The indent stack
     * @param globals The global parser state
     * @param emit The directives sink
     */
    void into(Stack stack, Globals globals, Emit emit);
}
