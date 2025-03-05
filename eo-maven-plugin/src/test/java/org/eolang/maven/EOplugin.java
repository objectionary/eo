/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import com.yegor256.farea.Farea;
import com.yegor256.farea.Plugin;
import java.io.IOException;

/**
 * Configures the EO Maven plugin within a {@link Farea}.
 * @since 0.52
 */
final class EOplugin {
    /**
     * The Farea object.
     */
    private final Farea farea;

    /**
     * Ctor.
     * @param farea The Farea
     */
    EOplugin(final Farea farea) {
        this.farea = farea;
    }

    /**
     * Appends the EO Maven plugin.
     * @return The plugin
     */
    public Plugin appendItself() throws IOException {
        return this.farea.build()
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
