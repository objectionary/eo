/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;

/**
 * Deleting the directory with files.
 *
 * @since 0.52
 */
final class CleanFiles {
    /**
     * The directory.
     */
    private final File directory;

    /**
     * Ctor.
     * @param dir Directory to be deleted
     */
    CleanFiles(final File dir) {
        this.directory = dir;
    }

    /**
     * Cleaning of the files.
     *
     * @return State {@code true} if deleted, {@code false} otherwise
     */
    public boolean clean() {
        return this.purge(this.directory);
    }

    /**
     * Recursive deletion.
     *
     * @param dir Directory to be deleted
     * @return State {@code true} if deleted, {@code false} otherwise
     */
    private boolean purge(final File dir) {
        final File[] contents = dir.listFiles();
        if (null != contents) {
            for (final File file : contents) {
                this.purge(file);
            }
        }
        final boolean state = dir.delete();
        if (state) {
            Logger.debug(this, "The directory %[file]s purged", dir);
        }
        return state;
    }
}
