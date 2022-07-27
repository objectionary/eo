/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

import com.yegor256.tojos.CachedTojos;
import com.yegor256.tojos.Csv;
import com.yegor256.tojos.Json;
import com.yegor256.tojos.Mono;
import com.yegor256.tojos.MonoTojos;
import com.yegor256.tojos.Tojos;
import java.nio.file.Path;
import java.util.Locale;

/**
 * Catalog with tojos, in some format.
 *
 * @since 0.22
 */
final class Catalog {

    /**
     * Path.
     */
    private final Path path;

    /**
     * Format.
     */
    private final String format;

    /**
     * Ctor.
     * @param file Path
     * @param fmt Format
     */
    Catalog(final Path file, final String fmt) {
        this.path = file;
        this.format = fmt;
    }

    /**
     * Make it.
     * @return The tojos
     */
    public Tojos make() {
        final String fmt = this.format.trim().toLowerCase(Locale.ENGLISH);
        final Mono mono;
        if ("json".equals(fmt)) {
            mono = new Json(this.path);
        } else if ("csv".equals(fmt)) {
            mono = new Csv(this.path);
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "Unrecognized format '%s' for the file '%s'",
                    fmt, this.path
                )
            );
        }
        return new CachedTojos(
            new MonoTojos(mono)
        );
    }
}
