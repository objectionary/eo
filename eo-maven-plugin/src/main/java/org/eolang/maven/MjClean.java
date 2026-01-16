/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Implementation of maven clean plugin,
 * just deleting target/eo directory.
 *
 * <p>
 *     This goal simply deletes the directory containing all the files used for compilation.
 * </p>
 *
 * @since 0.28.6
 */
@Mojo(
    name = "clean",
    defaultPhase = LifecyclePhase.CLEAN,
    threadSafe = true
)
public class MjClean extends MjSafe {

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
        if (new Deleted(this.targetDir).get()) {
            Logger.info(
                this,
                "Deleted all files in the %[file]s directory",
                this.targetDir
            );
        }
    }
}
