/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

package org.eolang.maven;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.scalar.IoChecked;

/**
 * Lowest level class for saving and loading files.
 * @since 0.27
 * @todo #1105:30min Add function for loading
 *  1) We should also be able to load file by path
 *  2) Unit tests for checking load function
 */
final class Disk {
    /**
     * Ctor.
     * @since 0.27
     */
    private Disk() { }

    /**
     * Saving.
     * @param path Path to file
     * @param stream Input stream
     * @return Saved bytes
     * @throws IOException If fails
     * @todo #1105:30min We need to modify function
     *  1) Edit file names (replace bad characters)
     *  2) Research if this way of writing is the good one.
     */
    static long save(final Path path, final IoChecked<InputStream> stream) throws IOException {
        Files.write(path, stream.value().readAllBytes());
        return stream.value().available();
    }
}
