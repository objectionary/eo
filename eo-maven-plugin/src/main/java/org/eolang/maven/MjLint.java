/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Mojo that runs all lints and checks errors and warnings,
 * preferably after the {@code assemble} goal.
 *
 * <p>This goal goes through all XMIR files generated in the previous steps (see {@link MjParse}
 * or {@link MjPull} goals) and runs all available lints on them.
 * If any errors or warnings are found, they are logged to the console,
 * and depending on the configuration, the build may fail.
 * The linting results are also embedded back into the XMIR files for future reference.
 * Lints might use caching to speed up the process on subsequent runs.
 * Cached files are stored in the {@link Linting#CACHE} directory.
 * The results of linting are saved in the {@link Linting#DIR} directory.</p>
 *
 * @since 0.31.0
 */
@Mojo(
    name = "lint",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjLint extends MjSafe {

    /**
     * Ctor.
     */
    public MjLint() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        try (
            TjsForeign tojos = this.tojos();
            TjsForeign compile = this.compileTojos()
        ) {
            new Linting(
                tojos,
                compile,
                this.target.toPath(),
                this.cache.toPath(),
                this.enabled,
                this.plugin.getVersion(),
                this.sourcelints,
                this.programlints,
                this.experimental,
                this.warning,
                this.pkg,
                this.linting
            ).exec();
        }
    }
}
