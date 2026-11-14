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
     * The source line shared by the head and its dispatch links.
     */
    private final int line;

    /**
     * The {@code .method} chain after the head (may be empty).
     */
    private final List<MethodChain> chain;

    /**
     * The optional attribute name for the final object.
     */
    private final String label;

    /**
     * Ctor.
     *
     * @param sink The directives sink
     * @param row The source line
     * @param links The dispatch chain
     * @param name Name for the final object, or {@code null}
     */
    ChainEmission(
        final Emit sink, final int row, final List<MethodChain> links, final String name
    ) {
        this.emit = sink;
        this.line = row;
        this.chain = links;
        this.label = name;
    }

    /**
     * Emit the head + chain. The cursor is left inside the last
     * opened {@code <o>} (head if no chain, last link if chained) so
     * the caller can attach horizontal args, then close.
     *
     * @param head The head value
     */
    void run(final Value head) {
        if (this.chain.isEmpty()) {
            Emissions.openValue(this.emit, this.label, head, this.line);
        } else {
            Emissions.openValue(this.emit, null, head, this.line);
        }
        this.append();
    }

    /**
     * Append dispatch links after the currently open head object.
     */
    void append() {
        final int last = this.chain.size() - 1;
        for (int idx = 0; idx <= last; idx = idx + 1) {
            final MethodChain chained = this.chain.get(idx);
            this.emit.close();
            if (idx == last) {
                this.emit.object(
                    this.label, ".".concat(chained.name()), this.line, chained.dot()
                );
            } else {
                this.emit.unnamedObject(".".concat(chained.name()), this.line, chained.dot());
            }
            this.emit.method(chained.fragile());
        }
    }
}
