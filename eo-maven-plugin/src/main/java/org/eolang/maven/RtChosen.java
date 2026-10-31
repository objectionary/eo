/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.apache.maven.project.MavenProject;
import org.cactoos.Scalar;

/**
 * The EO runtime dependency, from wherever the build has it.
 *
 * <p>The POM of the project wins, since a dependency spelled out there is
 * what the programmer asked for. Without one, the runtime comes from Maven
 * Central, or, when the build stays offline, from the version the plugin
 * itself was built with.</p>
 *
 * @since 0.62.0
 */
final class RtChosen implements Scalar<Dep> {

    /**
     * The project being built.
     */
    private final MavenProject project;

    /**
     * Whether Maven Central may be asked.
     */
    private final boolean central;

    /**
     * Ctor.
     *
     * @param project The project being built
     * @param central Whether Maven Central may be asked
     */
    RtChosen(final MavenProject project, final boolean central) {
        this.project = project;
        this.central = central;
    }

    @Override
    public Dep value() throws Exception {
        final Scalar<Dep> origin;
        final RtPom pom = new RtPom(this.project);
        if (pom.isPresent()) {
            origin = pom;
        } else if (this.central) {
            origin = new RtCentral();
        } else {
            origin = new RtOffline();
        }
        return origin.value();
    }
}
