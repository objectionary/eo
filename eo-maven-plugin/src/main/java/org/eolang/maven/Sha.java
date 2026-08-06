/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
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
 * For a directory, hashes every file sorted by its relative path, framed by that path and by
 * the amount of bytes read from it, so that trees differing in file names or in file boundaries
 * never collide. Only files speak into the digest, hence a directory holding none of them stays
 * invisible to the hash, the way Git also sees it.
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
     * Hashes all regular files reachable from the path, sorted by relative path.
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
                .sorted(Comparator.comparing(this::relative)).forEach(
                    file -> {
                        try (InputStream input = Files.newInputStream(file)) {
                            if (tree) {
                                digest.update(
                                    String.format("%s\0", this.relative(file))
                                        .getBytes(StandardCharsets.UTF_8)
                                );
                            }
                            final byte[] buffer = new byte[8192];
                            long length = 0L;
                            int read = input.read(buffer);
                            while (read != -1) {
                                digest.update(buffer, 0, read);
                                length = length + read;
                                read = input.read(buffer);
                            }
                            if (tree) {
                                digest.update(
                                    String.format("\0%d\0", length)
                                        .getBytes(StandardCharsets.UTF_8)
                                );
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

    /**
     * Path of the file relative to the hashed root, with forward slashes on every platform.
     * @param file File reachable from the root
     * @return Relative path of the file
     */
    private String relative(final Path file) {
        return this.path.relativize(file).toString().replace(File.separatorChar, '/');
    }
}
