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
package org.eolang.parser;

/**
 * Error message that includes the location of the error.
 * @since 0.50
 */
final class MsgLocated {

    /**
     * The line where the error occurred.
     */
    private final int line;

    /**
     * The position in the line where the error occurred.
     */
    private final int position;

    /**
     * The error message.
     */
    private final String message;

    /**
     * Ctor.
     * @param line The line where the error occurred.
     * @param position The position in the line where the error occurred.
     * @param message The error message.
     */
    MsgLocated(final int line, final int position, final String message) {
        this.line = line;
        this.position = position;
        this.message = message;
    }

    /**
     * Formats the error message.
     * @return The formatted error message.
     */
    String formatted() {
        return String.format("[%d:%d] error: '%s'", this.line, this.position, this.message);
    }
}
