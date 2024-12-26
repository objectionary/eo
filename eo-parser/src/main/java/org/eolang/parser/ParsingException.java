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

import java.util.List;

/**
 * When parsing fails.
 *
 * @since 0.1
 */
public final class ParsingException extends RuntimeException {

    /**
     * Serialization marker.
     */
    private static final long serialVersionUID = -3043426132301042201L;

    /**
     * The place.
     */
    private final int place;

    /**
     * Ctor.
     * @param cause Cause of failure
     * @param line The place
     * @param msgs Messages
     */
    ParsingException(final Exception cause, final int line, final String... msgs) {
        this(cause, line, List.of(msgs));
    }

    /**
     * Ctor.
     *
     * @param cause The cause
     * @param line The place
     * @param msgs Messages
     * @since 0.1
     */
    ParsingException(final Exception cause, final int line, final List<String> msgs) {
        this(cause, line, String.join("\n", msgs));
    }

    /**
     * Ctor.
     *
     * @param cause Cause of failure
     * @param line The place
     * @param msg Message
     */
    ParsingException(final Exception cause, final int line, final String msg) {
        super(msg, cause);
        this.place = line;
    }

    /**
     * Get the place.
     * @return Line
     */
    public int line() {
        return this.place;
    }
}
