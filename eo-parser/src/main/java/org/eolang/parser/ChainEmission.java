/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * The emission of a head expression plus its {@code .method} chain
 * (§9.0.3 of the spec): the head becomes the first sibling
 * {@code <o>}, each chain link emits as a separate
 * {@code <o base='.<name>' method=''>} sibling, and the line's name
 * suffix attaches to the last link (or the head when the chain is
 * empty).
 *
 * <p>Both {@link LnApplication} and {@link LnCompactTuple} share this
 * shape — the only thing that differs between them is the line-shape
 * classifier; once head/chain/suffix are parsed, the emission steps
 * are identical. This class is the single source of truth for those
 * steps.</p>
 *
 * @since 0.1
 */
final class ChainEmission {

    /**
     * The directives sink.
     */
    private final Emit emit;

    /**
     * The line's source span (for line/indent positions and the
     * name suffix's attribute computation).
     */
    private final Span span;

    /**
     * The head value (identifier, paren group, literal, {@code *},
     * root) — first sibling of the chain.
     */
    private final Value head;

    /**
     * The {@code .method} chain after the head (may be empty).
     */
    private final List<MethodChain> chain;

    /**
     * The line's name suffix — supplies the user-given attribute
     * name and the {@code !} const marker.
     */
    private final Suffix suffix;

    /**
     * Ctor.
     * @param sink The directives sink
     * @param src The line span (line + indent for positions)
     * @param start The head value of the chain
     * @param links The dispatch chain
     * @param sfx The parsed suffix
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    ChainEmission(
        final Emit sink, final Span src, final Value start,
        final List<MethodChain> links, final Suffix sfx
    ) {
        this.emit = sink;
        this.span = src;
        this.head = start;
        this.chain = links;
        this.suffix = sfx;
    }

    /**
     * Emit the head + chain. The cursor is left inside the last
     * opened {@code <o>} (head if no chain, last link if chained) so
     * the caller can attach horizontal args, then close.
     */
    void run() {
        final String name = this.suffix.attribute(
            this.span.line(), this.span.indent()
        );
        if (this.chain.isEmpty()) {
            Emissions.openValue(this.emit, name, this.head, this.span.line());
            if (this.suffix.constant()) {
                this.emit.constant();
            }
            if (this.head.oerr()) {
                this.emit.onError();
            }
        } else {
            Emissions.openValue(this.emit, null, this.head, this.span.line());
            this.emit.close();
            for (int idx = 0; idx < this.chain.size() - 1; idx = idx + 1) {
                final MethodChain link = this.chain.get(idx);
                Emissions.emitMethodLink(
                    this.emit, link, null, this.span.line()
                );
                this.emit.close();
            }
            Emissions.emitMethodLink(
                this.emit, this.chain.get(this.chain.size() - 1), name, this.span.line()
            );
            if (this.suffix.constant()) {
                this.emit.constant();
            }
        }
    }
}
