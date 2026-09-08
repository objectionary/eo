/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Compile and lint all EO files.
 *
 * <p>This goal combines {@link MjAssemble}, {@link MjLint}, {@link MjMerge},
 * {@link MjResolve} and {@link MjPlace} goals.
 * See their documentation to find out more details.
 * The {@link MjCompile} is useful to run the whole compilation process in one go without
 * the need to call each goal separately.</p>
 *
 * <p>The merge happens right after the lint, so that every goal between this
 * one and {@link MjTranspile}, such as {@link MjInference} or {@link MjLower},
 * reads the object in the shape it will be compiled in and not in the shape
 * the parser left. A project therefore never has to name {@code merge}
 * between them, and naming it changes nothing: a member already inside its
 * object is not moved again.</p>
 *
 * @since 0.52
 */
@Mojo(
    name = "compile",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjCompile extends MjSafe {

    /**
     * Ctor.
     */
    public MjCompile() {
        // nothing
    }

    @Override
    public void exec() throws IOException {
        try (
            TjsForeign tojos = this.tojos();
            TjsForeign compile = this.compileTojos();
            TjsPlaced placed = this.placed()
        ) {
            new Timed(
                new Compiling(
                    new Timed(this.assembling(tojos)),
                    new Timed(
                        new Linting(
                            tojos,
                            compile,
                            this.targetDir.toPath(),
                            this.cache.toPath(),
                            this.cacheEnabled,
                            this.plugin.getVersion(),
                            this.skipSourceLints,
                            this.skipProgramLints,
                            this.skipExperimental,
                            this.failOnWarning,
                            this.lintAsPackage,
                            this.skipLinting
                        )
                    ),
                    new Timed(
                        new Merging(
                            tojos,
                            this.targetDir.toPath().resolve(Merging.DIR)
                        )
                    ),
                    new Timed(
                        new Resolving(
                            tojos,
                            this.targetDir.toPath().resolve(MjResolve.DIR),
                            new CentralMaven(this.system, this.session, this.repositories),
                            this.discoverSelf,
                            this.skipZeroVersions,
                            this.resolveJna,
                            this.ignoreRuntime,
                            this.runtime(),
                            this.ignoreConflicts
                        )
                    ),
                    new Timed(
                        new Placing(
                            placed,
                            this.targetDir.toPath().resolve(MjResolve.DIR),
                            this.classesDir.toPath(),
                            this.placeBinaries,
                            this.skipBinaries,
                            this.rewriteBinaries
                        )
                    )
                )
            ).exec();
        }
    }
}
