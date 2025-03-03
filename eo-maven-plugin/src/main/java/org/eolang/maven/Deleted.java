/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
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
     * @param file Directory or file to be deleted
     */
    Deleted(final File file) {
        this.target = file;
    }

    @Override
    public Boolean get() {
        boolean state;
        if (this.target.isFile()) {
            try {
                Files.delete(this.target.toPath());
                Logger.debug(this, "The file %[file]s purged", this.target);
                state = true;
            } catch (final IOException ex) {
                Logger.warn(this, "Failed to delete file: %[file]s", this.target);
                state = false;
            }
        } else {
            state = this.purge(this.target);
        }
        return state;
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
