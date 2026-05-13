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
 * <p>
 *     This goal combines {@link MjAssemble}, {@link MjLint}, {@link MjResolve} and
 *     {@link MjPlace} goals.
 *     See their documentation to find out more details.
 *     The {@link MjCompile} is useful to run the whole compilation process in one go without
 *     the need to call each goal separately.
 * </p>
 *
 * @since 0.52
 */
@Mojo(
    name = "compile",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjCompile extends MjSafe {

    @Override
    public void exec() throws IOException {
        new Compiling(
            new Assembling(
                this.scopedTojos(),
                new Parsing(
                    this.scopedTojos(),
                    this.targetDir.toPath(),
                    this.cache.toPath(),
                    this.cacheEnabled,
                    this.plugin.getVersion(),
                    this.sourcesDir.toPath()
                ),
                new Probing(this.scopedTojos(), this.objectionary(), !this.offline),
                new Pulling(
                    this.scopedTojos(),
                    this.targetDir.toPath().resolve(Pulling.DIR),
                    this.hash,
                    this.objectionary(),
                    this.cache.toPath().resolve(Pulling.CACHE),
                    this.plugin.getVersion(),
                    this.overWrite,
                    this.cacheEnabled,
                    this.offline
                )
            ),
            new Linting(
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
                this.sourcesDir.toPath(),
                this.skipLinting
            ),
            new Resolving(
                this.scopedTojos(),
                this.targetDir.toPath().resolve(MjResolve.DIR),
                new CentralMaven(this.system),
                this.discoverSelf,
                this.skipZeroVersions,
                this.resolveJna,
                this.ignoreRuntime,
                this.runtime(),
                this.ignoreVersionConflicts
            ),
            new Placing(
                this.placedTojos,
                this.targetDir.toPath().resolve(MjResolve.DIR),
                this.classesDir.toPath(),
                this.placeBinaries,
                this.skipBinaries,
                this.rewriteBinaries
            )
        ).exec();
    }
}
