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
package org.eolang.maven;

import com.yegor256.tojos.MnCsv;
import com.yegor256.tojos.MnJson;
import com.yegor256.tojos.MnPostponed;
import com.yegor256.tojos.MnSticky;
import com.yegor256.tojos.Mono;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.Tojos;
import java.nio.file.Path;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * All catalogs in one place, to avoid making multiple objects.
 *
 * @since 0.29
 */
public final class Catalogs {

    /**
     * Singleton.
     */
    public static final Catalogs INSTANCE = new Catalogs();

    /**
     * Singleton.
     */
    private static final Unchecked<Boolean> TESTING = new Unchecked<>(
        new Sticky<>(
            () -> {
                synchronized (Catalogs.class) {
                    boolean tests;
                    try {
                        Class.forName("org.junit.jupiter.api.Test");
                        tests = true;
                    } catch (final ClassNotFoundException ex) {
                        tests = false;
                    }
                    return tests;
                }
            }
        )
    );

    /**
     * All of them.
     */
    private final ConcurrentHashMap<Path, Tojos> all;

    /**
     * Ctor.
     */
    private Catalogs() {
        this.all = new ConcurrentHashMap<>(0);
    }

    /**
     * Make it.
     * @param file The file
     * @return The Tojos
     */
    public Tojos make(final Path file) {
        return this.all.computeIfAbsent(
            file.toAbsolutePath(), f -> Catalogs.build(f, "csv")
        );
    }

    /**
     * Make it.
     * @param file The file
     * @param fmt The format
     * @return The Tojos
     */
    public Tojos make(final Path file, final String fmt) {
        return this.all.computeIfAbsent(
            file.toAbsolutePath(), f -> Catalogs.build(f, fmt)
        );
    }

    /**
     * Make it.
     * @param path Path of the file
     * @param format Format, like "csv" or "json"
     * @return The tojos
     */
    private static Tojos build(final Path path, final String format) {
        final String fmt = format.trim().toLowerCase(Locale.ENGLISH);
        Mono mono;
        if ("json".equals(fmt)) {
            mono = new MnJson(path);
        } else if ("csv".equals(fmt)) {
            mono = new MnCsv(path);
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "Unrecognized format '%s' for the file '%s'",
                    fmt, path
                )
            );
        }
        if (Catalogs.TESTING.value()) {
            mono = new MnSticky(mono);
        } else {
            mono = new MnPostponed(mono, 500L);
        }
        return new TjCached(new TjDefault(mono));
    }

}
