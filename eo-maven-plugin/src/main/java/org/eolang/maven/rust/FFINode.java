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
package org.eolang.maven.rust;

import java.io.IOException;
import java.io.UncheckedIOException;

/**
 * Common interface for FFI nodes in xmir.
 * @since 0.34
 * @checkstyle AbbreviationAsWordInNameCheck (8 lines)
 */
public interface FFINode {

    /**
     * Generate necessary files in target. For example, for rust nodes we need
     *  to create cargo project in target/Lib and java files in
     *  target/generated/EOrust/natives/
     * @throws IOException if any issues with IO.
     */
    void generate() throws IOException;

    /**
     * Generate method but throw unchecked IOException.
     */
    default void generateUnchecked() {
        try {
            this.generate();
        } catch (final IOException exc) {
            throw new UncheckedIOException(exc);
        }
    }
}
