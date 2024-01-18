/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang;

import java.util.regex.Pattern;

/**
 * Indented text.
 *
 * @since 0.1
 */
@Versionized
final class Indented {

    /**
     * Indenter.
     */
    private static final Pattern INDENT = Pattern.compile("\n");

    /**
     * Shift right.
     */
    private static final String SHIFT = "\n\t";

    /**
     * Original text.
     */
    private final Object text;

    /**
     * Ctor.
     * @param txt Text to indent
     */
    Indented(final Object txt) {
        this.text = txt;
    }

    @Override
    public String toString() {
        return Indented.INDENT.matcher(this.text.toString()).replaceAll(Indented.SHIFT);
    }

}
