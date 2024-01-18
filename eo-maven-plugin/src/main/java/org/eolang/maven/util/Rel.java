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

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Relative path.
 *
 * @since 0.28.11
 */
public final class Rel {

    /**
     * Current working directory.
     */
    private final Path base;

    /**
     * Path relative to working directory.
     */
    private final Path other;

    /**
     * Constructor with File.
     *
     * @param file File relative to working directory.
     */
    public Rel(final File file) {
        this(file.toPath());
    }

    /**
     * Constructor with Path.
     *
     * @param other Path relative to working directory.
     */
    public Rel(final Path other) {
        this(Paths.get(""), other);
    }

    /**
     * The main constructor.
     *
     * @param base Current working directory.
     * @param other Path relative to working directory.
     */
    public Rel(final Path base, final Path other) {
        this.base = base;
        this.other = other;
    }

    @Override
    public String toString() {
        String path = this.other.toAbsolutePath().toString();
        if (path.equals(this.base.toString())) {
            path = String.format(".%s", File.separator);
        } else if (path.startsWith(this.base.toString())) {
            path = String.format(
                ".%s%s",
                File.separator,
                path.substring(this.base.toString().length() + 1)
            );
        }
        return path;
    }
}
