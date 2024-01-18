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

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.Md5DigestOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.io.InputOf;

/**
 * MD5 hash of a file (its content).
 *
 * @since 0.24
 */
public final class FileHash {

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     * @param path The name of the file
     */
    public FileHash(final Path path) {
        this.file = path;
    }

    @Override
    public String toString() {
        final String hash;
        if (Files.exists(this.file)) {
            hash = Arrays.toString(
                new UncheckedBytes(
                    new Md5DigestOf(new InputOf(new BytesOf(this.file)))
                ).asBytes()
            );
        } else {
            hash = "";
        }
        return hash;
    }

}
