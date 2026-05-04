/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Optional;
import org.apache.maven.model.Dependency;
import org.apache.maven.project.MavenProject;
import org.cactoos.Scalar;

/**
 * Runtime dependency from the project POM file.
 * @since 0.62.0
 */
final class RtPom implements Scalar<Dep> {

    /**
     * Maven project.
     */
    private final MavenProject project;

    /**
     * Ctor.
     * @param mvn Maven project
     */
    RtPom(final MavenProject mvn) {
        this.project = mvn;
    }

    @Override
    public Dep value() {
        return this.runtimeFromPom()
            .map(Dep::new).orElseThrow(
                () -> new IllegalStateException("Runtime dependency not found in pom.xml")
            );
    }

    /**
     * Checks if runtime dependency is present in pom.xml.
     * @return True if runtime dependency found
     */
    boolean isPresent() {
        return this.runtimeFromPom().isPresent();
    }

    /**
     * Runtime dependency from pom.xml.
     * @return Dependency if found
     */
    private Optional<Dependency> runtimeFromPom() {
        final Optional<Dependency> res;
        if (this.project == null) {
            res = Optional.empty();
        } else {
            res = this.project
                .getDependencies()
                .stream()
                .filter(RtPom::isRuntime)
                .findFirst();
        }
        return res;
    }

    /**
     * Checks if dependency is the eo-runtime artifact.
     * @param dep Dependency
     * @return True if runtime
     */
    private static boolean isRuntime(final Dependency dep) {
        return "org.eolang".equals(dep.getGroupId())
            && "eo-runtime".equals(dep.getArtifactId());
    }
}
