/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Take binary files from where {@link MjResolve} placed them and
 * copy to the {@code target/classes} directory.
 *
 * <p>
 *     Input directory is {@link MjResolve#DIR}.
 *     Output directory is {@link MjPlace#targetDir}/classes.
 * </p>
 *
 * @see <a href="https://news.eolang.org/2022-10-19-placed-catalog.html">Place catalog</a>
 * @since 0.11
 */
@Mojo(
    name = "place",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjPlace extends MjSafe {

    @Override
    public void exec() throws IOException {
        new Placing(
            this.placedTojos,
            this.targetDir.toPath().resolve(MjResolve.DIR),
            this.classesDir.toPath(),
            this.placeBinaries,
            this.skipBinaries,
            this.rewriteBinaries
        ).exec();
    }
}
