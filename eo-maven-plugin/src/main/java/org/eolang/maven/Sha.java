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
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * SHA-256 hash of a file or directory.
 * For a directory, hashes the accepted files, sorted by relative path, framed by that path and by
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
     * Files that speak into the digest.
     */
    private final Predicate<Path> filter;

    /**
     * Ctor.
     * @param path File or directory to hash
     */
    Sha(final Path path) {
        this(path, file -> true);
    }

    /**
     * Ctor.
     * @param path File or directory to hash
     * @param filter Files that speak into the digest
     */
    Sha(final Path path, final Predicate<Path> filter) {
        this.path = path;
        this.filter = filter;
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
     * Hashes the accepted regular files reachable from the path, sorted by relative path.
     * A file of a directory is framed by its relative path and length, a lone file is not.
     * @return Base64-encoded SHA-256 hash
     * @throws IOException If reading fails
     * @throws NoSuchAlgorithmException If SHA-256 is unavailable
     */
    private String hash() throws IOException, NoSuchAlgorithmException {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        try (Stream<Path> walk = Files.walk(this.path)) {
            walk.filter(Files::isRegularFile)
                .filter(this.filter)
                .sorted(Comparator.comparing(this::relative))
                .forEach(file -> this.feed(digest, file));
        }
        return Base64.getEncoder().encodeToString(digest.digest());
    }

    /**
     * Feeds the bytes of the file into the digest, framed by the relative path of the file and
     * by the amount of bytes read, unless the file is the hashed path itself.
     * @param digest Digest to feed
     * @param file File to read
     */
    private void feed(final MessageDigest digest, final Path file) {
        final String relative = this.relative(file);
        try (InputStream input = Files.newInputStream(file)) {
            if (!relative.isEmpty()) {
                digest.update(
                    String.format("%s\0", relative).getBytes(StandardCharsets.UTF_8)
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
            if (!relative.isEmpty()) {
                digest.update(
                    String.format("\0%d\0", length).getBytes(StandardCharsets.UTF_8)
                );
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(String.format("Failed to read '%s'", file), ex);
        }
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
