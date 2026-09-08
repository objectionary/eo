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
     *
     * @param sink The directives sink
     * @param src The line span (line + indent for positions)
     * @param start The head value of the chain
     * @param links The dispatch chain
     * @param sfx The parsed suffix
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
        ChainEmission.link(
            this.emit, this.span.line(), this.head, this.chain,
            this.suffix.attribute(this.span.line(), this.span.indent())
        );
        if (!this.suffix.handle().isEmpty()) {
            this.emit.local(this.suffix.handle());
        }
        if (this.suffix.constant()) {
            this.emit.constant();
        }
    }

    /**
     * Emit the head + chain, leaving the last opened {@code <o>} open.
     *
     * @param sink The directives sink
     * @param line Source line
     * @param head The head value
     * @param links The dispatch chain (may be empty)
     * @param label Name for the last link, or {@code null}
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    static void link(
        final Emit sink, final int line, final Value head,
        final List<MethodChain> links, final String label
    ) {
        final int last = links.size() - 1;
        if (links.isEmpty()) {
            Emissions.openValue(sink, label, head, line);
        } else {
            Emissions.openValue(sink, null, head, line);
        }
        for (int idx = 0; idx <= last; idx = idx + 1) {
            final MethodChain chained = links.get(idx);
            sink.close();
            if (idx == last) {
                sink.object(label, ".".concat(chained.name()), line, chained.dot());
            } else {
                sink.unnamedObject(".".concat(chained.name()), line, chained.dot());
            }
            sink.method(chained.fragile());
        }
    }
}
