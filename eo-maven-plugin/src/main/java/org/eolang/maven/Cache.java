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
            final String hash = Cache.sha(source);
            final Path hfile = this.hash(tail);
            final Path cfile = this.base.resolve(tail);
            if (Files.notExists(hfile) || !Files.readString(hfile).equals(hash)) {
                final String content = new UncheckedFunc<>(this.compilation).apply(source);
                Files.writeString(this.hash(tail), hash);
                Files.writeString(cfile, content);
                Files.writeString(target, content);
            } else {
                Files.writeString(target, Files.readString(cfile));
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
        final Path full = this.base.resolve(tail);
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
        final byte[] hash = digest.digest(Files.readAllBytes(file));
        return Base64.getEncoder().encodeToString(hash);
    }
}

