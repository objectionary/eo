/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import com.jcabi.xml.XMLDocument;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.stream.Collectors;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.slf4j.impl.StaticLoggerBinder;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * Find all required runtime dependencies and add
 * them to classpath ("compile" scope).
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "resolve",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
public final class ResolveMojo extends AbstractMojo {

    /**
     * Maven project.
     */
    @Component
    private MavenProject project;

    /**
     * Maven session.
     */
    @Component
    private MavenSession session;

    /**
     * Maven plugin manager.
     */
    @Component
    private BuildPluginManager manager;

    /**
     * From directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo"
    )
    private File targetDir;

    /**
     * Output.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.outputDirectory}"
    )
    private File outputDirectory;

    @Override
    public void execute() throws MojoFailureException, MojoExecutionException {
        StaticLoggerBinder.getSingleton().setMavenLog(this.getLog());
        final Path dir = this.targetDir.toPath().resolve("03-optimize");
        final Collection<Dependency> deps;
        try {
            deps = Files.walk(dir)
                .filter(file -> !file.toFile().isDirectory())
                .map(this::artifacts)
                .flatMap(Collection::stream)
                .map(ResolveMojo.Wrap::new)
                .sorted()
                .distinct()
                .map(ResolveMojo.Wrap::dep)
                .collect(Collectors.toList());
        } catch (final IOException ex) {
            throw new MojoFailureException(
                String.format(
                    "Can't list XML files in %s",
                    dir
                ),
                ex
            );
        }
        Logger.info(this, "Found %d dependencie(s)", deps.size());
        for (final Dependency dep : deps) {
            this.unpack(dep);
        }
    }

    /**
     * Copy dependency and return its file name.
     * @param dep The dependency
     * @throws MojoExecutionException If fails
     */
    private void unpack(final Dependency dep) throws MojoExecutionException {
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
                        MojoExecutor.element("version", dep.getVersion()),
                        MojoExecutor.element("overWrite", "true"),
                        MojoExecutor.element(
                            "outputDirectory",
                            this.outputDirectory.toString()
                        )
                    )
                )
            ),
            MojoExecutor.executionEnvironment(
                this.project,
                this.session,
                this.manager
            )
        );
        Logger.info(
            this, "%s:%s:%s unpacked to %s",
            dep.getGroupId(), dep.getArtifactId(), dep.getVersion(),
            this.outputDirectory
        );
    }

    /**
     * Find the artifact required by this EO XML.
     *
     * @param file EO file
     * @return List of artifacts needed
     */
    private Collection<Dependency> artifacts(final Path file) {
        final Collection<Dependency> artifacts = new LinkedList<>();
        try {
            final String xpath = "//meta[head='rt' and part[1]='jvm']/part[2]/text()";
            for (final String coords : new XMLDocument(file).xpath(xpath)) {
                final String[] parts = coords.split(":");
                final Dependency dep = new Dependency();
                dep.setGroupId(parts[0]);
                dep.setArtifactId(parts[1]);
                dep.setVersion(parts[2]);
                dep.setClassifier("");
                dep.setType("jar");
                dep.setScope("compile");
                artifacts.add(dep);
            }
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "Can't pull %s into %s",
                    file, this.targetDir
                ),
                ex
            );
        }
        return artifacts;
    }

    /**
     * Wrapper for comparing.
     *
     * @since 0.1
     */
    private static final class Wrap implements Comparable<ResolveMojo.Wrap> {
        /**
         * Dependency.
         */
        private final Dependency dependency;

        /**
         * Ctor.
         * @param dep Dependency
         */
        private Wrap(final Dependency dep) {
            this.dependency = dep;
        }

        /**
         * Return it.
         * @return The dep
         */
        public Dependency dep() {
            return this.dependency;
        }

        @Override
        public int compareTo(final ResolveMojo.Wrap wrap) {
            return ResolveMojo.Wrap.toStr(this.dependency).compareTo(
                ResolveMojo.Wrap.toStr(wrap.dependency)
            );
        }

        @Override
        public boolean equals(final Object wrap) {
            return ResolveMojo.Wrap.toStr(this.dependency).equals(
                ResolveMojo.Wrap.toStr(
                    ResolveMojo.Wrap.class.cast(wrap).dependency
                )
            );
        }

        @Override
        public int hashCode() {
            return ResolveMojo.Wrap.toStr(this.dependency).hashCode();
        }

        /**
         * Convert it to string.
         * @param dep The dep
         * @return The text
         */
        private static String toStr(final Dependency dep) {
            return String.format(
                "%s:%s",
                dep.getGroupId(), dep.getArtifactId()
            );
        }
    }
}
