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
     * @param emt The directives sink
     * @param src The line span (line + indent for positions)
     * @param hed The head value
     * @param chn The dispatch chain
     * @param sfx The parsed suffix
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    ChainEmission(
        final Emit emt, final Span src, final Value hed,
        final List<MethodChain> chn, final Suffix sfx
    ) {
        this.emit = emt;
        this.span = src;
        this.head = hed;
        this.chain = chn;
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
        } else {
            Emissions.openValue(this.emit, null, this.head, this.span.line());
            this.emit.close();
            for (int idx = 0; idx < this.chain.size() - 1; idx = idx + 1) {
                final MethodChain link = this.chain.get(idx);
                this.emit.object(
                    null, ".".concat(link.name()), this.span.line(), link.dot()
                );
                this.emit.method();
                this.emit.close();
            }
            final MethodChain last = this.chain.get(this.chain.size() - 1);
            this.emit.object(
                name, ".".concat(last.name()), this.span.line(), last.dot()
            );
            this.emit.method();
            if (this.suffix.constant()) {
                this.emit.constant();
            }
        }
    }
}
