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
import org.cactoos.Scalar;

/**
 * JAR unpacked into a directory.
 * Rejects entries whose resolved path would leave the destination (Zip Slip).
 * @since 0.62.0
 */
final class Unpacked implements Scalar<Path> {

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
    Unpacked(final Path archive, final Path target) {
        this.jar = archive;
        this.dest = target;
    }

    @Override
    public Path value() throws IOException {
        final Path root = this.dest.toAbsolutePath().normalize();
        Files.createDirectories(root);
        try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(this.jar))) {
            ZipEntry entry = zip.getNextEntry();
            while (entry != null) {
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
                entry = zip.getNextEntry();
            }
        }
        return root;
    }
}
