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
package org.eolang.maven.util;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.io.InputOf;

/**
 * The data is saved to a file.
 * @since 0.32.0
 */
public interface DataSaved {
    /**
     * Saving string.
     *
     * @param str String
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    default void save(final String str, final Path path) throws IOException {
        this.save(new InputOf(str), path);
    }

    /**
     * Saving text.
     *
     * @param txt Text
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    default void save(final Text txt, final Path path) throws IOException {
        this.save(new InputOf(txt), path);
    }

    /**
     * Saving stream.
     *
     * @param stream Input stream
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    default void save(final InputStream stream, final Path path) throws IOException  {
        this.save(new InputOf(stream), path);
    }

    /**
     * Saving bytes.
     *
     * @param bytes Byte array
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     */
    default void save(final byte[] bytes, final Path path) throws IOException  {
        this.save(new InputOf(bytes), path);
    }

    /**
     * Saving input.
     *
     * @param input Input
     * @param path Cwd-relative path to file
     * @throws IOException If fails
     * @throws IllegalArgumentException If given path is absolute
     */
    void save(Input input, Path path) throws IOException;
}
