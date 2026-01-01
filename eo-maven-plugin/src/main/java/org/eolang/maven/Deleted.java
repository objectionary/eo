/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.util.function.Supplier;

/**
 * Deleting a directory with files or just a file.
 *
 * @since 0.52
 */
final class Deleted implements Supplier<Boolean> {
    /**
     * Target file or directory to be deleted.
     */
    private final File target;

    /**
     * Ctor.
     * @param file File or directory to be deleted
     */
    Deleted(final File file) {
        this.target = file;
    }

    @Override
    public Boolean get() {
        return this.purge(this.target);
    }

    /**
     * Recursive deletion.
     *
     * @param dir File or directory to be deleted
     * @return State {@code true} if deleted, {@code false} otherwise
     */
    private boolean purge(final File dir) {
        if (dir.isDirectory()) {
            final File[] contents = dir.listFiles();
            if (null != contents) {
                for (final File file : contents) {
                    this.purge(file);
                }
            }
        }
        final boolean state = dir.delete();
        if (state) {
            Logger.debug(this, "The file or directory %[file]s purged", dir);
        }
        return state;
    }
}
