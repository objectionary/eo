/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.function.BiConsumer;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * The abstraction of Maven Central.
 *
 * @since 0.1
 */
final class Central implements BiConsumer<Dependency, Path> {

    /**
     * Do nothing.
     */
    static final BiConsumer<Dependency, Path> EMPTY = new BiConsumer<>() {
        @Override
        public void accept(final Dependency dependency, final Path path) {
            assert dependency != null;
        }

        @Override
        public BiConsumer<Dependency, Path> andThen(
            final BiConsumer<? super Dependency, ? super Path> after
        ) {
            throw new UnsupportedOperationException("#andThen()");
        }
    };

    /**
     * Maven project.
     */
    private final MavenProject project;

    /**
     * Maven session.
     */
    private final MavenSession session;

    /**
     * Maven plugin manager.
     */
    private final BuildPluginManager manager;

    /**
     * Ctor.
     * @param prj Project
     * @param sess Session
     * @param mgr Manager
     */
    Central(final MavenProject prj, final MavenSession sess, final BuildPluginManager mgr) {
        this.project = prj;
        this.session = sess;
        this.manager = mgr;
    }

    @Override
    public void accept(final Dependency dep, final Path path) {
        try {
            MojoExecutor.executeMojo(
                MojoExecutor.plugin(
                    MojoExecutor.groupId("org.apache.maven.plugins"),
                    MojoExecutor.artifactId("maven-dependency-plugin"),
                    MojoExecutor.version("3.6.1")
                ),
                MojoExecutor.goal("unpack"),
                MojoExecutor.configuration(
                    MojoExecutor.element(
                        "artifactItems",
                        MojoExecutor.element(
                            "artifactItem",
                            MojoExecutor.element("groupId", dep.getGroupId()),
                            MojoExecutor.element("artifactId", dep.getArtifactId()),
                            MojoExecutor.element("classifier", dep.getClassifier()),
                            MojoExecutor.element("version", dep.getVersion()),
                            MojoExecutor.element("outputDirectory", path.toString())
                        )
                    )
                ),
                MojoExecutor.executionEnvironment(
                    this.project,
                    this.session,
                    this.manager
                )
            );
        } catch (final MojoExecutionException ex) {
            throw new IllegalStateException(ex);
        }
        if (dep.getClassifier() != null) {
            Logger.info(
                this, "%s:%s:%s:%s unpacked to %[file]s",
                dep.getGroupId(), dep.getArtifactId(), dep.getClassifier(), dep.getVersion(), path
            );
        } else {
            Logger.info(
                this, "%s:%s:%s unpacked to %[file]s",
                dep.getGroupId(), dep.getArtifactId(), dep.getVersion(), path
            );
        }
    }

    @Override
    public BiConsumer<Dependency, Path> andThen(
        final BiConsumer<? super Dependency, ? super Path> after
    ) {
        throw new UnsupportedOperationException("not implemented #andThen()");
    }
}
