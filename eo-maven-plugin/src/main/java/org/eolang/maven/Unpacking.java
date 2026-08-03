/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * Unpacks a JAR into a directory, rejecting Zip Slip paths.
 * @since 0.64
 */
final class Unpacking {

    /**
     * Path to the JAR file.
     */
    private final Path jar;

    /**
     * Destination directory.
     */
    private final Path dest;

    /**
     * Ctor.
     * @param archive Path to the JAR file
     * @param target Destination directory
     */
    Unpacking(final Path archive, final Path target) {
        this.jar = archive;
        this.dest = target;
    }

    /**
     * Unpack the archive into {@link #dest}.
     * @throws IOException If unpacking fails or an entry escapes {@link #dest}
     */
    void unpack() throws IOException {
        final Path root = this.dest.toAbsolutePath().normalize();
        Files.createDirectories(root);
        try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(this.jar))) {
            for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
                final Path target = root.resolve(entry.getName()).normalize();
                if (!target.startsWith(root)) {
                    throw new IOException(
                        String.format(
                            "Zip entry '%s' would write outside '%s'",
                            entry.getName(), root
                        )
                    );
                }
                if (entry.isDirectory()) {
                    Files.createDirectories(target);
                } else {
                    Files.createDirectories(target.getParent());
                    Files.copy(zip, target, StandardCopyOption.REPLACE_EXISTING);
                }
                zip.closeEntry();
            }
        }
    }
}
