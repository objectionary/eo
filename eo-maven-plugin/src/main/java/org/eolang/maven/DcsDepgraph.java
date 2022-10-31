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

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * Json File retrieved using the plugin for uploading transitive dependencies.
 * You can read more <a href="https://github.com/ferstl/depgraph-maven-plugin">here</a>
 *
 * @since 0.28.11
 */
final class DcsDepgraph implements Dependencies {

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
     * Dependency for which we are looking transitive dependencies.
     */
    private final Dependency dependency;

    /**
     * The main contructor.
     *
     * @param project Maven project
     * @param session Maven session
     * @param manager Maven plugin manager
     * @param dir Directory to save all transitive dependencies files
     * @param dependency Dependency
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    DcsDepgraph(
        final MavenProject project,
        final MavenSession session,
        final BuildPluginManager manager,
        final Path dir,
        final Dependency dependency
    ) {
        this.project = project;
        this.session = session;
        this.manager = manager;
        this.dir = dir;
        this.dependency = dependency;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return new DcsJson(this.file(this.dependency)).iterator();
    }

    /**
     * Receive file with dependencies jar file.
     *
     * @param origin Dependency
     * @return Path to the saved json dependency file
     */
    private Path file(final Dependency origin) {
        try {
            final String name = DcsDepgraph.fileName(origin);
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
            throw new IllegalStateException(
                String.format(
                    "Dphgraph. Creation of the dependencies file failed for the dependency %s",
                    origin
                ),
                ex
            );
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

    /**
     * Dependencies uploaded from json file.
     *
     * @since 0.28.11
     */
    static class DcsJson implements Dependencies {

        /**
         * File path.
         */
        private final Path file;

        /**
         * The main constructor.
         *
         * @param file File path
         */
        DcsJson(final Path file) {
            this.file = file;
        }

        @Override
        public Iterator<Dependency> iterator() {
            try {
                final List<Dependency> all = new ArrayList<>(0);
                if (Files.exists(this.file)) {
                    Logger.debug(this, String.format("Dependencies file: %s", this.file));
                    final JsonReader reader = Json.createReader(Files.newBufferedReader(this.file));
                    final JsonArray artifacts = reader.readObject()
                        .getJsonArray("artifacts");
                    for (final JsonValue artifact : artifacts) {
                        final JsonObject obj = artifact.asJsonObject();
                        final String group = obj.getString("groupId");
                        final String id = obj.getString("artifactId");
                        final String version = obj.getString("version");
                        final String scope = obj.getJsonArray("scopes").stream()
                            .map(JsonValue::toString)
                            .findFirst().orElseThrow(IllegalStateException::new);
                        final Dependency dependency = new Dependency();
                        dependency.setGroupId(group);
                        dependency.setArtifactId(id);
                        dependency.setVersion(version);
                        dependency.setScope(scope);
                        all.add(dependency);
                    }
                }
                return all.iterator();
            } catch (final IOException | IllegalStateException ex) {
                throw new IllegalStateException(
                    String.format(
                        "Exception occurred during reading of the dependencies from the json file %s",
                        this.file
                    ),
                    ex
                );
            }
        }
    }
}
