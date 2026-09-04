/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
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
 * For a directory, hashes every file sorted by its relative path, framed by that path and by
 * the amount of bytes read from it, so that trees differing in file names or in file boundaries
 * never collide. Only files speak into the digest, hence a directory holding none of them stays
 * invisible to the hash, the way Git also sees it.
 * The walk follows symbolic links, because everything else here already does:
 * a path is called a directory with {@link Files#isDirectory(Path, java.nio.file.LinkOption...)}
 * and a file with {@link Files#isRegularFile(Path, java.nio.file.LinkOption...)}, and both look
 * through a link. Without that, a link to a directory was walked but never entered, so the digest
 * was the one of empty input, the same for every such link and unchanged by anything behind it.
 * @since 0.62.0
 */
final class Sha {

    /**
     * File or directory to hash.
     */
    private final Path path;

    /**
     * Files filter, applied only when hashing a directory.
     */
    private final Predicate<Path> filter;

    /**
     * Ctor.
     * @param path File or directory to hash
     */
    Sha(final Path path) {
        this(path, p -> true);
    }

    /**
     * Ctor.
     * @param path File or directory to hash
     * @param filter Files filter, applied only when hashing a directory
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

    private String hash() throws IOException, NoSuchAlgorithmException {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        final Predicate<Path> active;
        if (Files.isDirectory(this.path)) {
            active = this.filter;
        } else {
            active = any -> true;
        }
        try (Stream<Path> walk = Files.walk(this.path, FileVisitOption.FOLLOW_LINKS)) {
            walk.filter(Files::isRegularFile)
                .filter(active)
                .sorted(Comparator.comparing(this::relative))
                .forEach(file -> this.feed(digest, file));
        }
        return Base64.getEncoder().encodeToString(digest.digest());
    }

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

    private String relative(final Path file) {
        return this.path.relativize(file).toString().replace(File.separatorChar, '/');
    }
}
