/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Implementation of maven clean plugin,
 * just deleting target/eo directory.
 *
 * @since 0.28.6
 */
@Mojo(
    name = "clean",
    defaultPhase = LifecyclePhase.CLEAN,
    threadSafe = true
)
public class CleanMojo extends SafeMojo {

    @Override
    final void exec() {
        if (!this.targetDir.exists()) {
            Logger.debug(
                this,
                "The directory %[file]s doesn't exist",
                this.targetDir
            );
            return;
        }
        if (this.purge(this.targetDir)) {
            Logger.info(
                this,
                "Deleted all files in the %[file]s directory",
                this.targetDir
            );
        }
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
