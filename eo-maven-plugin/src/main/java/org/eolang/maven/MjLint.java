/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Mojo that runs all lints and checks errors and warnings,
 * preferably after the {@code assemble} goal.
 * <p>
 *     This goal goes through all XMIR files generated in the previous steps (see {@link MjParse}
 *     or {@link MjPull} goals) and runs all available lints on them.
 *     If any errors or warnings are found, they are logged to the console,
 *     and depending on the configuration, the build may fail.
 *     The linting results are also embedded back into the XMIR files for future reference.
 *     Lints might use caching to speed up the process on subsequent runs.
 *     Cached files are stored in the {@link Lint#CACHE} directory.
 *     The results of linting are saved in the {@link Lint#DIR} directory.
 * </p>
 * @since 0.31.0
 */
@Mojo(
    name = "lint",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjLint extends MjSafe {

    @Override
    void exec() throws IOException {
        if (this.skipLinting) {
            Logger.info(this, "Linting is skipped because eo:skipLinting is TRUE");
        } else {
            new Lint(
                this.scopedTojos(),
                this.compileTojos(),
                this.targetDir.toPath(),
                this.cache.toPath(),
                this.cacheEnabled,
                this.plugin.getVersion(),
                this.skipSourceLints,
                this.skipProgramLints,
                this.skipExperimentalLints,
                this.failOnWarning,
                this.lintAsPackage,
                this.sourcesDir.toPath()
            ).exec();
        }
    }
}
