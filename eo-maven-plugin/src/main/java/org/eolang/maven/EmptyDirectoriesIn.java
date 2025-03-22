/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Delete empty directories in provided root.
 *
 * @since 0.55
 */
final class EmptyDirectoriesIn {
    /**
     * Root path.
     */
    private final Path root;

    /**
     * Ctor.
     * @param root Root directory.
     */
    EmptyDirectoriesIn(final Path root) {
        this.root = root;
    }

    /**
     * Clear empty directories in {@code this.root}.
     */
    void clear() {
        if (Files.isDirectory(this.root)) {
            this.delete(this.root.toFile());
        } else {
            throw new IllegalStateException(
                Logger.format("Provided path %[file]s is not a directory", this.root)
            );
        }
    }

    /**
     * Recursively delete empty directories.
     * @param dir Directory to delete
     * @checkstyle NestedIfDepthCheck (20 lines)
     */
    private void delete(final File dir) {
        if (dir.isDirectory()) {
            final File[] before = dir.listFiles();
            if (before != null) {
                for (final File file : before) {
                    if (file.isDirectory()) {
                        this.delete(file);
                    }
                }
            }
            final File[] after = dir.listFiles();
            if (after != null
                && after.length == 0
                && !dir.equals(this.root.toFile())
                && dir.delete()
            ) {
                Logger.debug(EmptyDirectoriesIn.class, "Deleted empty directory %[file]s", dir);
            }
        }
    }
}
