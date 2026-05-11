/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 * Parse EO to XML.
 *
 * <p>
 *     This is the initial goal that parses all found EO sources to XMIRs.
 *     You can read more about XMIR format
 *     <a href="https://www.eolang.org/XMIR.html">here</a>
 * </p>
 * <p>
 *    The goal scans all the EO sources registered in the foreign file catalog
 *    (see {@link MjRegister} and {@link MjPull}) and then parses those that were not parsed
 *    before (i.e. do not have XMIRs yet) to XMIR format.
 *    The resulting XMIR files are stored in the {@link Parsing#DIR} directory.
 * </p>
 *
 * @since 0.1
 */
@Mojo(
    name = "parse",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class MjParse extends MjSafe {

    @Override
    public void exec() {
        new Parsing(
            this.scopedTojos(),
            this.targetDir.toPath(),
            this.cache.toPath(),
            this.cacheEnabled,
            this.plugin.getVersion(),
            this.sourcesDir.toPath()
        ).exec();
    }
}
