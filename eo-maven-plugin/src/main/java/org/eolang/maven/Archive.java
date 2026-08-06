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
 * @since 0.64.0
 */
final class Archive {

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
        final Path root = dest.toAbsolutePath().normalize();
        Files.createDirectories(root);
        final Collection<Path> written = new ArrayList<>(0);
        try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(this.file))) {
            for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
                final Path target = root.resolve(entry.getName()).normalize();
                if (!target.startsWith(root)) {
                    Archive.cleanup(written);
                    throw new IOException(
                        String.format(
                            "Zip entry '%s' would write outside '%s'",
                            entry.getName(), root
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
