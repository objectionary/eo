/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.manifests.Manifests;
import com.yegor256.farea.Farea;
import com.yegor256.farea.Plugin;
import java.io.IOException;

/**
 * The eo-maven-plugin appended to {@link Farea}.
 * @since 0.54
 */
final class EoMavenPlugin {
    /**
     * Farea.
     */
    private final Farea farea;

    /**
     * Ctor.
     * @param farea Farea
     */
    EoMavenPlugin(final Farea farea) {
        this.farea = farea;
    }

    /**
     * Append eo-maven-plugin to farea build.
     * @return Appended eo-maven-plugin
     * @throws IOException If fails to append
     */
    Plugin appended() throws IOException {
        return this.farea
            .build()
            .plugins()
            .append(
                "org.eolang",
                "eo-maven-plugin",
                System.getProperty(
                    "eo.version",
                    Manifests.read("EO-Version")
                )
            );
    }
}
