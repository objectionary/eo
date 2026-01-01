/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.tojos.MnCsv;
import com.yegor256.tojos.MnJson;
import com.yegor256.tojos.MnPostponed;
import com.yegor256.tojos.MnSticky;
import com.yegor256.tojos.Mono;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.TjSynchronized;
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
final class Catalogs {

    /**
     * Singleton.
     */
    static final Catalogs INSTANCE = new Catalogs();

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
    Tojos make(final Path file) {
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
    Tojos make(final Path file, final String fmt) {
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
        return new TjSynchronized(new TjCached(new TjDefault(mono)));
    }
}
