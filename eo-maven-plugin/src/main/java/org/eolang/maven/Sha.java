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
import java.util.stream.Stream;

/**
 * SHA-256 hash of a file or directory.
 * For a directory, hashes every file sorted by path, framed by its relative path and length,
 * so that trees differing in file names or in file boundaries never collide.
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

    @Override
    public String toString() {
        try {
            return this.hash();
        } catch (final IOException | NoSuchAlgorithmException ex) {
            throw new IllegalStateException("Failed to compute SHA-256 hash", ex);
        }
    }

    /**
     * Hashes all regular files reachable from the path, sorted by path name.
     * A file of a directory is framed by its relative path and length, a lone file is not.
     * @return Base64-encoded SHA-256 hash
     * @throws IOException If reading fails
     * @throws NoSuchAlgorithmException If SHA-256 is unavailable
     */
    private String hash() throws IOException, NoSuchAlgorithmException {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        final boolean tree = Files.isDirectory(this.path);
        try (Stream<Path> walk = Files.walk(this.path)) {
            walk.filter(Files::isRegularFile)
                .sorted(Comparator.comparing(Path::toString)).forEach(
                    file -> {
                        try (InputStream input = Files.newInputStream(file)) {
                            if (tree) {
                                digest.update(
                                    String.format(
                                        "%s\0%d\0", this.path.relativize(file), Files.size(file)
                                    ).getBytes(StandardCharsets.UTF_8)
                                );
                            }
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
