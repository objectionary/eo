/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * A JAR (ZIP) archive that can extract itself into a directory.
 * Rejects Zip Slip paths that would escape the destination.
 * Limits decompressed output to prevent zip bomb attacks.
 * @since 0.64.0
 */
final class Archive {

    /**
     * Maximum total decompressed bytes (100 MB).
     */
    private static final long MAX_DECOMPRESSED_SIZE = 100L * 1024L * 1024L;

    /**
     * Path to the archive file.
     */
    private final Path file;

    /**
     * Ctor.
     * @param path Path to the archive file
     */
    Archive(final Path path) {
        this.file = path;
    }

    /**
     * Extract entries into {@code dest}, refusing anything outside it.
     * If a Zip Slip entry is detected partway through, already-extracted
     * entries are deleted so the destination is left clean.
     * @param dest Destination directory
     * @throws IOException If extraction fails or an entry escapes {@code dest}
     */
    void extract(final Path dest) throws IOException {
        final Path root = dest.toAbsolutePath();
        // Resolve real path to detect and reject symlinks in the destination
        final Path realRoot = root.toRealPath();
        Files.createDirectories(realRoot);
        final Collection<Path> written = new ArrayList<>(0);
        long decompressed = 0L;
        try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(this.file))) {
            for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
                // Check decompressed size limit
                if (entry.getSize() > 0L) {
                    decompressed += entry.getSize();
                    if (decompressed > Archive.MAX_DECOMPRESSED_SIZE) {
                        Archive.cleanup(written);
                        throw new IOException(
                            String.format(
                                "Decompressed size exceeds limit of %d bytes",
                                Archive.MAX_DECOMPRESSED_SIZE
                            )
                        );
                    }
                }
                // Normalize and check that the resolved target stays within the root
                final Path target = realRoot.resolve(entry.getName()).normalize();
                if (!target.startsWith(realRoot)) {
                    Archive.cleanup(written);
                    throw new IOException(
                        String.format(
                            "Zip entry '%s' would write outside '%s'",
                            entry.getName(), realRoot
                        )
                    );
                }
                if (entry.isDirectory()) {
                    Files.createDirectories(target);
                    written.add(target);
                } else {
                    Files.createDirectories(target.getParent());
                    Files.copy(zip, target, StandardCopyOption.REPLACE_EXISTING);
                    written.add(target);
                }
                zip.closeEntry();
            }
        }
    }

    /**
     * Delete every path that was already written, best-effort.
     * @param paths Paths to delete
     */
    private static void cleanup(final Collection<Path> paths) {
        for (final Path path : paths) {
            try {
                Files.deleteIfExists(path);
            } catch (final IOException ignored) {
                // best-effort cleanup
            }
        }
    }
}
