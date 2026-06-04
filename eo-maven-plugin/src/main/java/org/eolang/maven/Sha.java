/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import org.cactoos.Scalar;

/**
 * SHA-256 hash of a stream or file.
 * @since 0.62.0
 */
final class Sha {

    /**
     * Source of bytes to hash.
     */
    private final Scalar<InputStream> source;

    /**
     * Ctor.
     * @param stream Input stream to hash
     */
    Sha(final InputStream stream) {
        this(() -> stream);
    }

    /**
     * Ctor.
     * @param file File to hash
     */
    Sha(final Path file) {
        this(() -> Files.newInputStream(file));
    }

    /**
     * Primary ctor.
     * @param source Source of bytes to hash
     */
    private Sha(final Scalar<InputStream> source) {
        this.source = source;
    }

    /**
     * Returns Base64-encoded SHA-256 hash of the bytes.
     * @return Base64-encoded SHA-256 hash
     */
    @Override
    public String toString() {
        try {
            final MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (InputStream stream = this.source.value()) {
                final byte[] buffer = new byte[8192];
                int read = stream.read(buffer);
                while (read != -1) {
                    digest.update(buffer, 0, read);
                    read = stream.read(buffer);
                }
            }
            return Base64.getEncoder().encodeToString(digest.digest());
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 algorithm is not available", ex);
        } catch (final Exception ex) {
            throw new IllegalStateException("Failed to compute SHA-256 hash", ex);
        }
    }
}
