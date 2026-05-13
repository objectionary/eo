/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * It deletes binary files, which were previously copied by "place" mojo so
 * these binaries are not got into result JAR.
 * @since 0.11
 */
@Mojo(
    name = "unplace",
    defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
    threadSafe = true
)
public final class MjUnplace extends MjSafe {

    @Override
    public void exec() throws IOException {
        new Unplacing(
            this.placedTojos,
            this.classesDir.toPath(),
            this.keepBinaries
        ).exec();
    }
}
