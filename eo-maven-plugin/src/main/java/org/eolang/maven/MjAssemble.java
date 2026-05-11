/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Pull all necessary EO XML files from Objectionary and parse them all.
 *
 * <p>
 *     This goal combines {@link MjParse}, {@link MjProbe} and {@link MjPull}.
 *     See their documentation to find out more details.
 *     The {@link MjAssemble} runs these goals repeatedly until no new objects are
 *     pulled or parsed in a cycle.
 * </p>
 *
 * @since 0.1
 */
@Mojo(
    name = "assemble",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjAssemble extends MjSafe {

    /**
     * The intermediate representation extension.
     */
    static final String XMIR = "xmir";

    /**
     * Source file extension.
     */
    static final String EO = "eo";

    @Override
    public void exec() throws IOException {
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
        ).exec();
    }
}
