/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.text.TextOf;

/**
 * Source text without the byte order mark some editors write in front of it.
 *
 * <p>A UTF-8 byte order mark (U+FEFF) is not part of the program. Left where
 * it is, it stands in front of the first line, which then matches no line
 * shape at all, and every line after it is judged against the wrong one,
 * while the caret of each error points at a character no editor shows. It is
 * taken off here, the way a carriage return is, so that a file saved on
 * Windows parses (R-2.1.3).</p>
 *
 * @since 0.0.0
 */
final class Unmarked implements Text {

    /**
     * The mark, as Unicode numbers it.
     */
    private static final String MARK = Character.toString(0xFEFF);

    /**
     * The text to read.
     */
    private final Text origin;

    /**
     * Ctor.
     *
     * @param input The source to read the text from
     */
    Unmarked(final Input input) {
        this(new TextOf(input));
    }

    /**
     * Ctor.
     *
     * @param text The text to take the mark off
     */
    Unmarked(final Text text) {
        this.origin = text;
    }

    @Override
    public String asString() throws Exception {
        final String text = this.origin.asString();
        final String result;
        if (text.startsWith(Unmarked.MARK)) {
            result = text.substring(Unmarked.MARK.length());
        } else {
            result = text;
        }
        return result;
    }
}
