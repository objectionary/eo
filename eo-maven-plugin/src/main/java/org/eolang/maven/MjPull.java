/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Pull EO files from Objectionary.
 * <p>
 *     This goal goes through all objects from "foreign" catalog and looks for those without
 *     sources and pulls them from Objectionary remote repository.
 *     The pulled sources are stored in the {@link Pull#DIR} directory.
 * </p>
 * @since 0.1
 */
@Mojo(
    name = "pull",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjPull extends MjSafe {

    @Override
    public void exec() throws IOException {
        new Pull(
            this.scopedTojos(),
            this.targetDir.toPath().resolve(Pull.DIR),
            this.hash,
            this.objectionary(),
            this.cache.toPath().resolve(Pull.CACHE),
            this.plugin.getVersion(),
            this.overWrite,
            this.cacheEnabled,
            this.offline
        ).exec();
    }
}
