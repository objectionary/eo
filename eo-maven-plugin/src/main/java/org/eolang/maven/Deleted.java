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

    // The answer of a child is the answer of the parent too: a directory that
    // still holds a file nobody could delete is not deleted either, and the
    // caller has to hear about it instead of reading a clean build that kept
    // the files of the previous one (#8146).
    private boolean purge(final File dir) {
        boolean state = true;
        if (dir.isDirectory()) {
            final File[] contents = dir.listFiles();
            if (null != contents) {
                for (final File file : contents) {
                    state &= this.purge(file);
                }
            }
        }
        state &= dir.delete();
        if (state) {
            Logger.debug(this, "The file or directory %[file]s purged", dir);
        }
        return state;
    }
}
