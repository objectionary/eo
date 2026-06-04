/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Comparator;
import java.util.stream.Stream;

/**
 * SHA-256 hash of a file or directory.
 * For a directory, walks all files sorted by path and hashes their combined contents.
 * @since 0.62.0
 */
final class Sha {

    /**
     * File or directory to hash.
     */
    private final Path path;

    /**
     * Ctor.
     * @param path File or directory to hash
     */
    Sha(final Path path) {
        this.path = path;
    }

    /**
     * Returns Base64-encoded SHA-256 hash.
     * @return Base64-encoded SHA-256 hash
     */
    @Override
    public String toString() {
        try {
            return Sha.hash(this.path);
        } catch (final IOException | NoSuchAlgorithmException ex) {
            throw new IllegalStateException("Failed to compute SHA-256 hash", ex);
        }
    }

    /**
     * Hashes all regular files reachable from the given path, sorted by path name.
     * Works for both a single file and a directory.
     * @param path File or directory
     * @return Base64-encoded SHA-256 hash
     * @throws IOException If reading fails
     * @throws NoSuchAlgorithmException If SHA-256 is unavailable
     */
    private static String hash(final Path path) throws IOException, NoSuchAlgorithmException {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        try (Stream<Path> walk = Files.walk(path)) {
            walk.filter(Files::isRegularFile)
                .sorted(Comparator.comparing(Path::toString))
                .forEach(
                    file -> {
                        try (InputStream input = Files.newInputStream(file)) {
                            final byte[] buffer = new byte[8192];
                            int read = input.read(buffer);
                            while (read != -1) {
                                digest.update(buffer, 0, read);
                                read = input.read(buffer);
                            }
                        } catch (final IOException ex) {
                            throw new IllegalStateException(
                                String.format("Failed to read '%s'", file), ex
                            );
                        }
                    }
                );
        }
        return Base64.getEncoder().encodeToString(digest.digest());
    }
}
