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
package org.eolang.maven.tojos;

import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * Transpiled tojos that keeps information about all transpiled files.
 *
 * @since 0.30
 */
public final class TranspiledTojos implements Closeable {

    /**
     * All tojos.
     */
    private final Unchecked<? extends Tojos> all;

    /**
     * Current object lock that is used to synchronize access to the object.
     */
    private final Object lock;

    /**
     * The main public constructor.
     * @param tojos Tojos source.
     */
    public TranspiledTojos(final Sticky<? extends Tojos> tojos) {
        this(new Unchecked<>(tojos));
    }

    /**
     * Ctor.
     * @param tojos Tojos source.
     */
    TranspiledTojos(final Tojos tojos) {
        this(new Sticky<>(() -> tojos));
    }

    /**
     * The main constructor.
     * @param tojos Tojos source.
     */
    TranspiledTojos(final Unchecked<? extends Tojos> tojos) {
        this.all = tojos;
        this.lock = new Object();
    }

    @Override
    public void close() throws IOException {
        this.all.value().close();
    }

    /**
     * Add transpiled file to the list.
     * @param transpiled Transpiled file.
     * @param optimized Optimized xmir file.
     */
    public void add(final Path transpiled, final Path optimized) {
        synchronized (this.lock) {
            this.all.value().add(String.valueOf(transpiled)).set(
                Attribute.OPTIMIZED.key(),
                optimized
            );
        }
    }

    /**
     * Remove all transpiled files by xmir.
     * @param optimized Optimized xmir file.
     * @return Number of removed files.
     */
    public long remove(final Path optimized) {
        return this.findByOptimized(optimized)
            .stream()
            .map(row -> row.get(Attribute.ID.key()))
            .map(File::new)
            .filter(File::delete)
            .count();
    }

    /**
     * Find all tojos by optimized xmir.
     * @param optimized Optimized xmir file.
     * @return List of tojos.
     */
    private List<Tojo> findByOptimized(final Path optimized) {
        synchronized (this.lock) {
            return this.all.value().select(
                row -> row.get(Attribute.OPTIMIZED.key()).equals(optimized.toString())
            );
        }
    }

    /**
     * All possible attributes of transpiled tojos.
     * It's convenient to keep them encapsulated.
     *
     * @since 0.30
     */
    private enum Attribute {
        /**
         * Id.
         */
        ID("id"),

        /**
         * Optimized xmir.
         */
        OPTIMIZED("optimized");

        /**
         * Attribute key in tojos file.
         */
        private final String key;

        /**
         * The main constructor.
         * @param attribute Attribute key in tojos file.
         */
        Attribute(final String attribute) {
            this.key = attribute;
        }

        /**
         * Get attribute key.
         * @return Attribute key.
         */
        String key() {
            return this.key;
        }
    }
}
