/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * The decoded bytes of a literal that may carry escape sequences.
 *
 * <p>{@link Escapes} raises {@link NumberFormatException} for four
 * distinct conditions, each carrying its own text naming the offending
 * characters. This reports that text as a {@link ParseError} at the
 * literal's own line and column, so the XMIR {@code <error>} says which
 * escape failed instead of a single blanket message.</p>
 *
 * @since 0.1
 */
final class Unescaped {

    /**
     * The literal's text, without its surrounding quotes.
     */
    private final String text;

    /**
     * Source line of the literal.
     */
    private final int line;

    /**
     * Column position of the literal.
     */
    private final int pos;

    /**
     * Ctor.
     *
     * @param raw The literal's text, without its surrounding quotes
     * @param row Source line of the literal
     * @param col Column position of the literal
     */
    Unescaped(final String raw, final int row, final int col) {
        this.text = raw;
        this.line = row;
        this.pos = col;
    }

    /**
     * The decoded bytes.
     *
     * @return Bytes the literal stands for
     */
    byte[] bytes() {
        final byte[] decoded;
        try {
            decoded = Escapes.bytes(this.text);
        } catch (final NumberFormatException ex) {
            final ParseError error = new ParseError(this.line, this.pos, ex.getMessage());
            error.initCause(ex);
            throw error;
        }
        return decoded;
    }
}
