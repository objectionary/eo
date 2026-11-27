/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * One link in a {@code .method.method} chain — §3.5, §3.6.
 *
 * <p>Captures the method name and the source column of the leading
 * dot. Per R-9.1.3, the dot column is the {@code @pos} value emitted
 * for the link's {@code <o>} (not the column of the method name).</p>
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
     * Whether this link is a fragile dispatch ({@code ?.} instead of
     * {@code .}) — R-3.5 / §9.4.
     */
    private final boolean fragile;

    /**
     * Ctor.
     *
     * @param ident Method name
     * @param dot Column of the dot
     * @param weak Whether the link is a fragile {@code ?.} dispatch
     */
    MethodChain(final String ident, final int dot, final boolean weak) {
        this.name = ident;
        this.dot = dot;
        this.fragile = weak;
    }

    /**
     * Emit this link's object and dispatch markers.
     *
     * @param sink Directives sink
     * @param line Source line
     * @param label Name of the link, or {@code null} when unnamed
     */
    void emit(final Emit sink, final int line, final String label) {
        sink.object(label, ".".concat(this.name), line, this.dot);
        sink.method(this.fragile);
    }
}
