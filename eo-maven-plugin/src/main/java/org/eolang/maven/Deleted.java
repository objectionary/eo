/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.LinkOption;
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

    // A symbolic link is an entry of this directory, not the directory it
    // points at: walking through it would delete files that no build of ours
    // ever wrote (#8145). Only a real directory is descended into, and the
    // link itself goes as a leaf, which is what File.delete does to it.
    private boolean purge(final File dir) {
        if (Files.isDirectory(dir.toPath(), LinkOption.NOFOLLOW_LINKS)) {
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
