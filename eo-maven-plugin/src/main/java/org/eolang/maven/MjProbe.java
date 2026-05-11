/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Go through all `probe` and `also` metas in XMIR files, try to locate the
 * objects pointed by `probe` in Objectionary, and if found, register them in
 * the catalog.
 * More about the purpose of this Mojo is in
 * <a href="https://github.com/objectionary/eo/issues/1323">this issue</a>.
 *
 * <p>
 *     This goal just modifies the "foreign" catalog by adding newly discovered
 *     objects to it. It does not download or pull the sources of these objects,
 *     that is the job of {@link MjPull} goal which usually goes after this one.
 *     This goal does not create any files on the disk either.
 * </p>
 *
 * @since 0.28.11
 */
@Mojo(
    name = "probe",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjProbe extends MjSafe {

    @Override
    public void exec() throws IOException {
        new Probing(this.scopedTojos(), this.objectionary(), !this.offline).exec();
    }
}
