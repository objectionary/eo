/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
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
        this.base = base;
        this.compilation = compilation;
    }

    /**
     * Check cache and apply compilation if needed.
     * @param source From file
     * @param target To file
     * @param tail Tail path in cache
     */
    public void apply(final Path source, final Path target, final Path tail) {
        try {
            final String sha = Cache.sha(source);
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
        } catch (final NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 hashing algorithm isn't found", exception);
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
     * Calculate SHA-256 hash of a file and return it as Base64 string.
     * @param file File path
     * @return Base64-encoded SHA-256 hash
     * @throws NoSuchAlgorithmException If SHA-256 algorithm is not available
     * @throws IOException If an I/O error occurs reading the file
     */
    private static String sha(final Path file) throws NoSuchAlgorithmException, IOException {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        try (var stream = Files.newInputStream(file)) {
            final byte[] buffer = new byte[8192];
            int read = stream.read(buffer);
            while (read != -1) {
                digest.update(buffer, 0, read);
                read = stream.read(buffer);
            }
        }
        final byte[] hash = digest.digest();
        return Base64.getEncoder().encodeToString(hash);
    }
}

