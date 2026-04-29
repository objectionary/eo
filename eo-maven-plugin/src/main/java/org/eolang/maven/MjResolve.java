/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.Scalar;

/**
 * Find all required runtime dependencies, download
 * them from Maven Central, unpack and place to the {@code target/eo}
 * directory.
 *
 * <p>
 *     The motivation for this mojo is simple: Maven doesn't have
 *     a mechanism for adding .JAR files to transpile/test classpath in
 *     runtime.
 * </p>
 *
 * <p>
 *     This goal goes through all dependencies found in the
 *     {@link MjPull} goal, finds their implementations
 *     (i.e. transitive dependencies), downloads them from Maven Central,
 *     unpacks them and places the resulting files to the
 *     {@link MjResolve#DIR} directory.
 * </p>
 * @since 0.1
 */
@Mojo(
    name = "resolve",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjResolve extends MjSafe {

    /**
     * The directory where to resolve to.
     */
    static final String DIR = "4-resolve";

    /**
     * The central.
     *
     * @checkstyle MemberNameCheck (5 lines)
     */
    private BiConsumer<Dependency, Path> central;

    /**
     * Resolve default JNA dependency or not.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private boolean resolveJna = true;

    /**
     * Resolve dependencies in central or not.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private boolean resolveInCentral = true;

    @Override
    public void exec() {
        if (this.central == null) {
            this.central = new CentralMaven(this.system);
        }
        new Resolve(
            this.scopedTojos(),
            this.targetDir.toPath().resolve(MjResolve.DIR),
            this.central,
            this.discoverSelf,
            this.skipZeroVersions,
            this.resolveJna,
            this.ignoreRuntime,
            this.runtime(),
            this.ignoreVersionConflicts
        ).exec();
    }

    private Scalar<Dep> runtime() {
        final Scalar<Dep> result;
        final RtPom pom = new RtPom(this.project);
        if (pom.isPresent()) {
            result = pom;
        } else if (this.resolveInCentral) {
            result = new RtCentral();
        } else {
            result = new RtOffline();
        }
        return result;
    }
}
