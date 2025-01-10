/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import java.util.Collections;

/**
 * Underlined message.
 * <p>
 * For example, if you have a message "Problem is here" and you want to underline
 * the word "is", you can create an instance of this class with the following
 * parameters: origin="Problem is here", from=8, length=2.
 * </p>
 * <p>
 * The result will be:
 * {@code
 * Problem is here
 *         ^^
 * }
 * </p>
 * @since 0.50
 */
final class MsgUnderlined {

    /**
     * The message.
     */
    private final String origin;

    /**
     * The position from which to start underlining.
     */
    private final int from;

    /**
     * The length of the underline.
     */
    private final int length;

    /**
     * Ctor.
     * @param origin The message.
     * @param from The position from which to start underlining.
     * @param length The length of the underline.
     */
    MsgUnderlined(final String origin, final int from, final int length) {
        this.origin = origin;
        this.from = from;
        this.length = length;
    }

    /**
     * Formatted message.
     * @return The formatted message.
     */
    String formatted() {
        return String.format(
            "%s\n%s",
            this.origin,
            this.underline()
        );
    }

    /**
     * Underline.
     * @return The underlined string.
     */
    private String underline() {
        final String result;
        if (this.origin.isEmpty() || this.length <= 0 || this.from >= this.origin.length()) {
            result = "";
        } else if (this.from < 0) {
            result = MsgUnderlined.repeat("^", this.origin.length());
        } else {
            result = String.format(
                "%s%s",
                MsgUnderlined.repeat(" ", this.from),
                MsgUnderlined.repeat("^", Math.min(this.length, this.origin.length()))
            );
        }
        return result;
    }

    /**
     * Repeat a symbol n times.
     * @param symbol The symbol.
     * @param times The number of times to repeat the symbol.
     * @return The repeated symbol.
     */
    private static String repeat(final String symbol, final int times) {
        return String.join("", Collections.nCopies(times, symbol));
    }
}
