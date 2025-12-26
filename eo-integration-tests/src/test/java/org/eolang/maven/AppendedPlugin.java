/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import com.yegor256.farea.Execution;
import com.yegor256.farea.Farea;
import java.io.IOException;
import org.cactoos.Scalar;

/**
 * Configures the EO Maven plugin within a {@link Farea}.
 * @since 0.52
 * @todo #4777:90min Continue moving integration tests from eo-maven-plugin to
 * eo-integration-tests module. You can recognize these tests by the -IT suffix.
 * They use Farea to run Maven builds with eo-maven-plugin.
 * After moving all tests, remove the duplicated
 * {@link AppendedPlugin} class from eo-maven-plugin module.
 * Don't forget to remove exclusion from 'simian.yaml' as well.
 */
final class AppendedPlugin implements Scalar<Execution> {
    /**
     * The Farea object.
     */
    private final Farea farea;

    /**
     * Ctor.
     * @param farea The Farea
     */
    AppendedPlugin(final Farea farea) {
        this.farea = farea;
    }

    @Override
    public Execution value() throws IOException {
        return this.farea.build()
            .plugins()
            .append(
                "org.eolang",
                "eo-maven-plugin",
                System.getProperty(
                    "eo.version",
                    Manifests.read("EO-Version")
                )
            ).execution();
    }
}
