/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
final class TjsTranspiled implements Closeable {

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
    TjsTranspiled(final Sticky<? extends Tojos> tojos) {
        this(new Unchecked<>(tojos));
    }

    /**
     * Ctor.
     * @param tojos Tojos source.
     */
    TjsTranspiled(final Tojos tojos) {
        this(new Sticky<>(() -> tojos));
    }

    /**
     * The main constructor.
     * @param tojos Tojos source.
     */
    TjsTranspiled(final Unchecked<? extends Tojos> tojos) {
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
    void add(final Path transpiled, final Path optimized) {
        synchronized (this.lock) {
            this.all.value().add(String.valueOf(transpiled)).set(
                Attribute.OPTIMIZED.getKey(),
                optimized
            );
        }
    }

    /**
     * Remove all transpiled files by xmir.
     * @param optimized Optimized xmir file.
     * @return Number of removed files.
     */
    long remove(final Path optimized) {
        return this.findByOptimized(optimized)
            .stream()
            .map(row -> row.get(Attribute.ID.getKey()))
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
                row -> row.get(Attribute.OPTIMIZED.getKey()).equals(optimized.toString())
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
        String getKey() {
            return this.key;
        }
    }
}
