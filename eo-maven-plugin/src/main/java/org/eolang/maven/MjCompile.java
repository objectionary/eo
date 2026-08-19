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
                        )
                    ),
                    new Timed(
                        new Resolving(
                            tojos,
                            this.target.toPath().resolve(MjResolve.DIR),
                            new CentralMaven(this.system, this.session, this.repositories),
                            this.discover,
                            this.zeroversions,
                            this.jna,
                            this.runtime,
                            this.runtime(),
                            this.conflicts
                        )
                    ),
                    new Timed(
                        new Placing(
                            placed,
                            this.target.toPath().resolve(MjResolve.DIR),
                            this.classes.toPath(),
                            this.placing,
                            this.excluded,
                            this.rewrite
                        )
                    )
                )
            ).exec();
        }
    }
}
