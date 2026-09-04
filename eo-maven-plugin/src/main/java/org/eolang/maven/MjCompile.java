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
 * <p>This goal combines {@link MjAssemble}, {@link MjLint}, {@link MjResolve} and
 * {@link MjPlace} goals.
 * See their documentation to find out more details.
 * The {@link MjCompile} is useful to run the whole compilation process in one go without
 * the need to call each goal separately.</p>
 *
 * @since 0.52
 * @todo #6659:30min Let this goal merge the packages too. Since the merge
 *  became mandatory, a build that does not run {@link MjMerge} between this
 *  goal and {@link MjTranspile} names the atom of a package member after the
 *  member, while the runtime jar, merged when it was built, carries the name
 *  the merge gives it, and the generated Java does not compile. Every project
 *  has to list {@code merge} of its own accord today, this one and the README
 *  of the plugin included. Chain it here, once it is safe to run it twice,
 *  since {@code eo-runtime} lists it after this goal already.
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
