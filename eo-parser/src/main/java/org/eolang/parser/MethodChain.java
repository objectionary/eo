/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * One link in a {@code .method.method} chain — §3.5, §3.6.
 *
 * <p>Captures the method name, the source column of the leading dot, and
 * whether the dispatch is fragile. Per R-9.1.3, the dot column is the
 * {@code @pos} value emitted for the link's {@code <o>}.</p>
 *
 * @since 0.1
 */
final class MethodChain {

    /**
     * Method name without the leading dot.
     */
    private final String name;

    /**
     * Column of the leading dot.
     */
    private final int dot;

    /**
     * Whether this link is a fragile dispatch ({@code ?.}).
     */
    private final boolean fragile;

    /**
     * Ctor.
     *
     * @param ident Method name
     * @param pos Column of the dot
     * @param weak Whether the link is a fragile {@code ?.} dispatch
     */
    MethodChain(final String ident, final int pos, final boolean weak) {
        this.name = ident;
        this.dot = pos;
        this.fragile = weak;
    }

    /**
     * Write this method link to the emitter.
     *
     * @param sink Directives sink
     * @param line Source line
     * @param label Optional name for the emitted link
     */
    void write(final Emit sink, final int line, final String label) {
        sink.object(label, ".".concat(this.name), line, this.dot);
        sink.method(this.fragile);
    }
}
