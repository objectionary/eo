/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
 * <p>A symbolic link is a leaf here, whatever stands at the end of it. EO
 * may remove a directory it made and then emptied, but the tree behind a
 * link is somebody else's: descending through one would delete directories
 * outside the build output that EO never created. A link is also something
 * a directory holds, so a directory holding one is not empty.</p>
 *
 * <p>This holds for the root as well as for anything under it: when the
 * root given is a link, the walk stops there and the tree it points at is
 * left alone.</p>
 *
 * @since 0.55
 */
final class EmptyDirectoriesIn {

    /**
     * Root path.
     */
    private final File root;

    /**
     * Ctor.
     *
     * @param root Root directory
     */
    EmptyDirectoriesIn(final Path root) {
        this(root.toFile());
    }

    /**
     * Ctor.
     *
     * @param root Root directory
     */
    EmptyDirectoriesIn(final File root) {
        this.root = root;
    }

    /**
     * Clear empty directories in {@code this.root}.
     */
    void clear() {
        if (!this.root.isDirectory()) {
            throw new IllegalStateException(
                Logger.format("Provided path %[file]s is not a directory", this.root)
            );
        }
        this.delete(this.root);
    }

    private void delete(final File dir) {
        if (!dir.isDirectory() || Files.isSymbolicLink(dir.toPath())) {
            return;
        }
        final File[] before = dir.listFiles();
        if (before != null) {
            for (final File file : before) {
                this.delete(file);
            }
        }
        final File[] after = dir.listFiles();
        if (after != null && after.length == 0 && !dir.equals(this.root) && dir.delete()) {
            Logger.debug(EmptyDirectoriesIn.class, "Deleted empty directory %[file]s", dir);
        }
    }
}
