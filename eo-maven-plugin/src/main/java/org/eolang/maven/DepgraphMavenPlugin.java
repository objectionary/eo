/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.nio.file.Path;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * Plugin for uploading transitive dependencies.
 * You can read more <a href="https://github.com/ferstl/depgraph-maven-plugin">here</a>
 *
 * @since 0.28.11
 */
final class DepgraphMavenPlugin implements DependenciesPlugin {

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
     * Directory to save all transitive dependencies files.
     */
    private final Path dir;

    /**
     * The main contructor.
     *
     * @param project Maven project
     * @param session Maven session
     * @param manager Maven plugin manager
     * @param dir Directory to save all transitive dependencies files
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    DepgraphMavenPlugin(
        final MavenProject project,
        final MavenSession session,
        final BuildPluginManager manager,
        final Path dir
    ) {
        this.project = project;
        this.session = session;
        this.manager = manager;
        this.dir = dir;
    }

    @Override
    public Path dependencies(final Dependency origin) {
        try {
            final String name = DepgraphMavenPlugin.fileName(origin);
            MojoExecutor.executeMojo(
                MojoExecutor.plugin(
                    MojoExecutor.groupId("com.github.ferstl"),
                    MojoExecutor.artifactId("depgraph-maven-plugin"),
                    MojoExecutor.version("4.0.2")
                ),
                MojoExecutor.goal("for-artifact"),
                MojoExecutor.configuration(
                    MojoExecutor.element("groupId", origin.getGroupId()),
                    MojoExecutor.element("artifactId", origin.getArtifactId()),
                    MojoExecutor.element("version", origin.getVersion()),
                    MojoExecutor.element("graphFormat", "json"),
                    MojoExecutor.element("outputDirectory", this.dir.toString()),
                    MojoExecutor.element("outputFileName", name)
                ),
                MojoExecutor.executionEnvironment(
                    this.project,
                    this.session,
                    this.manager
                )
            );
            return this.dir.resolve(name);
        } catch (final MojoExecutionException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Creates filename for transitive dependencies file.
     *
     * @param dependency Dependency
     * @return Filename
     */
    private static String fileName(final Dependency dependency) {
        return String.format(
            "%s_%s_%s%s",
            dependency.getGroupId(),
            dependency.getArtifactId(),
            dependency.getVersion(),
            ".json"
        );
    }
}
