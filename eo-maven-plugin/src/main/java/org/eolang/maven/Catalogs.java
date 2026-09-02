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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;

/**
 * All catalogs in one place, to avoid making multiple objects.
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
        new Synced<>(
            new Sticky<>(
                () -> {
                    boolean tests;
                    try {
                        Class.forName("org.junit.jupiter.api.Test");
                        tests = true;
                    } catch (final ClassNotFoundException ex) {
                        tests = false;
                    }
                    return tests;
                }
            )
        )
    );

    /**
     * All of them.
     */
    private final Map<Path, Tojos> all;

    /**
     * The format each cached path was first built with.
     */
    private final Map<Path, String> formats;

    /**
     * Ctor.
     */
    private Catalogs() {
        this.all = new ConcurrentHashMap<>(0);
        this.formats = new ConcurrentHashMap<>(0);
    }

    /**
     * Make it.
     * @param file The file
     * @return The Tojos
     */
    Tojos make(final Path file) {
        return this.make(file, "csv");
    }

    /**
     * Make it.
     * @param file The file
     * @param fmt The format
     * @return The Tojos
     */
    Tojos make(final Path file, final String fmt) {
        final Path abs = file.toAbsolutePath();
        final String format = fmt.trim().toLowerCase(Locale.ENGLISH);
        final String before = this.formats.putIfAbsent(abs, format);
        if (before != null && !before.equals(format)) {
            throw new IllegalStateException(
                String.format(
                    "The file '%s' was already cached with format '%s', can't reuse it with format '%s'",
                    abs, before, format
                )
            );
        }
        return this.all.computeIfAbsent(abs, f -> Catalogs.build(f, format));
    }

    private static Tojos build(final Path path, final String fmt) {
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
