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
 * A JAR (ZIP) file, unpacked into a destination directory.
 * @since 0.64
 */
final class UnpackedJar {

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
     * @param src Path to the JAR file
     * @param target Destination directory
     */
    UnpackedJar(final Path src, final Path target) {
        this.jar = src;
        this.dest = target;
    }

    /**
     * Unpack the JAR into the destination directory.
     * @throws IOException If unpacking fails
     */
    void unpack() throws IOException {
        Files.createDirectories(this.dest);
        final Path home = this.dest.normalize();
        try (ZipInputStream zis = new ZipInputStream(Files.newInputStream(this.jar))) {
            ZipEntry entry = zis.getNextEntry();
            while (entry != null) {
                final Path target = this.dest.resolve(entry.getName()).normalize();
                if (!target.startsWith(home)) {
                    throw new IOException(
                        String.format(
                            "Zip entry '%s' would unpack to '%s', outside of '%s'",
                            entry.getName(), target, home
                        )
                    );
                }
                if (entry.isDirectory()) {
                    Files.createDirectories(target);
                } else {
                    Files.createDirectories(target.getParent());
                    Files.copy(zis, target, StandardCopyOption.REPLACE_EXISTING);
                }
                zis.closeEntry();
                entry = zis.getNextEntry();
            }
        }
    }
}
