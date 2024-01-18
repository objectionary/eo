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
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * Copied resources via maven-resources-plugin.
 * @since 0.33.0
 */
public final class CopiedResources implements BiConsumer<Path, Path> {
    /**
     * Maven executor environment.
     */
    private final MojoExecutor.ExecutionEnvironment environment;

    /**
     * Ctor.
     * @param project Maven project.
     * @param session Maven session.
     * @param manager Maven build manager.
     */
    public CopiedResources(
        final MavenProject project,
        final MavenSession session,
        final BuildPluginManager manager
    ) {
        this.environment = MojoExecutor.executionEnvironment(
            project,
            session,
            manager
        );
    }

    @Override
    public void accept(final Path sources, final Path destination) {
        final String src = sources.toString();
        final String dst = destination.toString();
        try {
            MojoExecutor.executeMojo(
                MojoExecutor.plugin(
                    MojoExecutor.groupId("org.apache.maven.plugins"),
                    MojoExecutor.artifactId("maven-resources-plugin")
                ),
                MojoExecutor.goal("copy-resources"),
                MojoExecutor.configuration(
                    MojoExecutor.element("outputDirectory", dst),
                    MojoExecutor.element(
                        "resources",
                        MojoExecutor.element(
                            "resource",
                            MojoExecutor.element("directory", src),
                            MojoExecutor.element("filtering", "true")
                        )
                    )
                ),
                this.environment
            );
        } catch (final MojoExecutionException ex) {
            throw new IllegalStateException(
                String.format("Couldn't copy resources from %s to %s", src, dst), ex
            );
        }
        Logger.info(this, "Resources from %s were copied to %s", src, dst);
    }

    @Override
    public BiConsumer<Path, Path> andThen(
        final BiConsumer<? super Path, ? super Path> after) {
        throw new UnsupportedOperationException("not implemented #andThen()");
    }
}
