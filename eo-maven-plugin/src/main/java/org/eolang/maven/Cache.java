/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Comparator;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.cactoos.Func;
import org.cactoos.func.UncheckedFunc;

/**
 * Simple cache mechanism.
 * This class isn't thread-safe, use {@link ConcurrentCache} for concurrent scenarios.
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
     * @param path Cache path
     * @param compilation Compilation function
     */
    Cache(final CachePath path, final Func<Path, String> compilation) {
        this(path.get(), compilation);
    }

    /**
     * Ctor.
     * @param base Base cache directory
     * @param compilation Compilation function
     */
    Cache(final Path base, final Func<Path, String> compilation) {
        this(base, compilation, p -> true);
    }

    /**
     * Constructor.
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
                new Saved(sha, this.hash(tail)).value();
                new Saved(content, cache).value();
                new Saved(content, target).value();
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

    /**
     * Get hash file path for the given tail.
     * @param tail Tail path
     * @return Hash file path
     */
    private Path hash(final Path tail) {
        final Path full = this.base.resolve(tail.normalize());
        return full.getParent().resolve(String.format("%s.sha256", full.getFileName().toString()));
    }

    /**
     * Calculate SHA-256 hash of a file or directory.
     * @param any File or directory path
     * @return Base64-encoded SHA-256 hash of the file or directory contents
     */
    private String sha(final Path any) {
        final String result;
        if (Files.isDirectory(any)) {
            result = this.dirSha(any);
        } else if (Files.isRegularFile(any)) {
            result = Cache.fileSha(any);
        } else {
            throw new IllegalArgumentException(
                String.format("Path '%s' is neither a regular file nor a directory", any)
            );
        }
        return result;
    }

    /**
     * Calculate SHA-256 hash of a directory by hashing all regular files inside it.
     * @param dir Directory path
     * @return Base64-encoded SHA-256 hash of the directory contents
     */
    private String dirSha(final Path dir) {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (Stream<Path> stream = Files.walk(dir)) {
                stream.filter(Files::isRegularFile)
                    .filter(this.filter::test)
                    .sorted(Comparator.comparing(Path::toString))
                    .map(Cache::fileSha)
                    .map(s -> s.getBytes(StandardCharsets.UTF_8))
                    .forEach(digest::update);
            }
            return Base64.getEncoder().encodeToString(digest.digest());
        } catch (final NoSuchAlgorithmException exception) {
            throw new IllegalStateException(
                "SHA-256 algorithm is not available for dir hashing",
                exception
            );
        } catch (final IOException exception) {
            throw new IllegalStateException(
                String.format("Failed to read directory '%s' for hashing", dir),
                exception
            );
        }
    }

    /**
     * Calculate SHA-256 hash of a file and return it as Base64 string.
     * @param file File path
     * @return Base64-encoded SHA-256 hash
     */
    private static String fileSha(final Path file) {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (InputStream stream = Files.newInputStream(file)) {
                final byte[] buffer = new byte[8192];
                int read = stream.read(buffer);
                while (read != -1) {
                    digest.update(buffer, 0, read);
                    read = stream.read(buffer);
                }
            }
            return Base64.getEncoder().encodeToString(digest.digest());
        } catch (final NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 algorithm is not available", exception);
        } catch (final IOException exception) {
            throw new IllegalStateException(
                String.format("Failed to read file '%s' for hashing", file),
                exception
            );
        }
    }
}
