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
 * for the link's {@code <o>} (not the column of the method name). *
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
     * Index past this link in the source body.
     */
    private final int end;

    /**
     * Ctor.
     * @param ident Method name
     * @param dot Column of the dot
     * @param after Index past this link
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    MethodChain(final String ident, final int dot, final int after) {
        this.name = ident;
        this.dot = dot;
        this.end = after;
    }

    /**
     * Method name (no leading dot).
     * @return Name
     */
    String name() {
        return this.name;
    }

    /**
     * Column of the leading dot.
     * @return Dot column
     */
    int dot() {
        return this.dot;
    }

    /**
     * Index past this link.
     * @return End index
     */
    int end() {
        return this.end;
    }
}
