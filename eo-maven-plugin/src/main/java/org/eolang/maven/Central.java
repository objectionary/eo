/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
import org.eolang.maven.util.Rel;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * The abstraction of Maven Central.
 *
 * @since 0.1
 */
public final class Central implements BiConsumer<Dependency, Path> {

    /**
     * Do nothing.
     */
    public static final BiConsumer<Dependency, Path> EMPTY = new BiConsumer<Dependency, Path>() {
        @Override
        public void accept(final Dependency dependency, final Path path) {
            assert dependency != null;
        }

        @Override
        public BiConsumer<Dependency, Path> andThen(
            final BiConsumer<? super Dependency, ? super Path> after) {
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
    public Central(final MavenProject prj, final MavenSession sess,
        final BuildPluginManager mgr) {
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
                    MojoExecutor.artifactId("maven-dependency-plugin")
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
        Logger.info(
            this, "%s:%s:%s:%s unpacked to %s",
            dep.getGroupId(), dep.getArtifactId(), dep.getClassifier(), dep.getVersion(),
            new Rel(path)
        );
    }

    @Override
    public BiConsumer<Dependency, Path> andThen(
        final BiConsumer<? super Dependency, ? super Path> after) {
        throw new UnsupportedOperationException("not implemented #andThen()");
    }
}
