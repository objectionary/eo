/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Predicate;
import org.cactoos.Func;
import org.cactoos.func.UncheckedFunc;

/**
 * Simple cache mechanism.
 * This class isn't thread-safe, use {@link ConcurrentCache} for concurrent scenarios.
 *
 * @since 0.60
 */
final class Cache {

    /**
     * Base cache directory.
     */
    private final Path base;

    /**
     * Compilation function.
     */
    private final Func<Path, String> compilation;

    /**
     * Files filter for dir cache.
     */
    private final Predicate<Path> filter;

    /**
     * Constructor.
     *
     * @param path Cache path
     * @param compilation Compilation function
     */
    Cache(final CachePath path, final Func<Path, String> compilation) {
        this(path.get(), compilation);
    }

    /**
     * Ctor.
     *
     * @param base Base cache directory
     * @param compilation Compilation function
     */
    Cache(final Path base, final Func<Path, String> compilation) {
        this(base, compilation, p -> true);
    }

    /**
     * Constructor.
     *
     * @param base Base cache directory
     * @param compilation Compilation function
     * @param filter Filter for files
     */
    Cache(
        final Path base,
        final Func<Path, String> compilation,
        final Predicate<Path> filter
    ) {
        this.base = base;
        this.compilation = compilation;
        this.filter = filter;
    }

    /**
     * Check cache and apply compilation if needed.
     *
     * @param source From file
     * @param target To file
     * @param tail Tail path in cache
     */
    void apply(final Path source, final Path target, final Path tail) {
        try {
            final String sha = this.sha(source);
            final Path hash = this.hash(tail);
            final Path cache = this.base.resolve(tail);
            if (
                Files.notExists(hash)
                    || Files.notExists(cache)
                    || !Files.readString(hash).equals(sha)
            ) {
                final String content = new UncheckedFunc<>(this.compilation).apply(source);
                new Saved(content, cache).value();
                new Saved(content, target).value();
                new Saved(sha, this.hash(tail)).value();
            } else {
                new Saved(Files.readString(cache), target).value();
            }
        } catch (final IOException ioexception) {
            throw new IllegalStateException(
                "Failed to perform an IO operation with cache",
                ioexception
            );
        }
    }

    private Path hash(final Path tail) {
        final Path full = this.base.resolve(tail.normalize());
        return full.getParent().resolve(String.format("%s.sha256", full.getFileName().toString()));
    }

    private String sha(final Path any) {
        if (!Files.isDirectory(any) && !Files.isRegularFile(any)) {
            throw new IllegalArgumentException(
                String.format("Path '%s' is neither a regular file nor a directory", any)
            );
        }
        return new Sha(any, this.filter).toString();
    }
}
