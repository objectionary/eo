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
package org.eolang.maven.dependencies;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
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
 * A list of transitive dependencies for a given Maven dependency.
 *
 * A JSON File is retrieved using the plugin for uploading
 * transitive dependencies. Then, we go through the JSON, parse it
 * and build a list of dependencies.
 *
 * @see <a href="https://github.com/ferstl/depgraph-maven-plugin">here</a>
 * @since 0.28.11
 * @checkstyle NoJavadocForOverriddenMethodsCheck (200 lines)
 */
public final class DcsDepgraph implements Iterable<Dependency> {

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
     * The main constructor.
     *
     * @param pkt Maven project
     * @param ssn Maven session
     * @param mgr Maven plugin manager
     * @param path Directory to save all transitive dependencies files
     * @param dep Dependency
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    public DcsDepgraph(
        final MavenProject pkt,
        final MavenSession ssn,
        final BuildPluginManager mgr,
        final Path path,
        final Dependency dep
    ) {
        this.project = pkt;
        this.session = ssn;
        this.manager = mgr;
        this.dir = path;
        this.dependency = dep;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return new DcsDepgraph.DcsJson(this.file(this.dependency)).iterator();
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
     * @param dep Dependency
     * @return Filename
     */
    private static String fileName(final Dependency dep) {
        return String.format(
            "%s_%s_%s%s",
            dep.getGroupId(),
            dep.getArtifactId(),
            dep.getVersion(),
            ".json"
        );
    }

    /**
     * Dependencies uploaded from json file.
     *
     * @since 0.28.11
     */
    static final class DcsJson implements Iterable<Dependency> {

        /**
         * File path.
         */
        private final Path file;

        /**
         * The main constructor.
         *
         * @param path File path
         */
        DcsJson(final Path path) {
            this.file = path;
        }

        /**
         * Returns an iterator over elements of type {@code T}.
         *
         * @return Iterator.
         * @todo #1897:30m Close `JsonReader` object in `DcsJson`.
         *  SonarCloud mandates readers to be closed.
         *  Use try-with-resources or close this "JsonReader" in a "finally" clause. (line 203)
         */
        @Override
        public Iterator<Dependency> iterator() {
            try {
                final Collection<Dependency> all = new ArrayList<>(0);
                if (Files.exists(this.file)) {
                    Logger.debug(this, String.format("Dependencies file: %s", this.file));
                    final JsonArray artifacts;
                    try (JsonReader reader =
                        Json.createReader(Files.newBufferedReader(this.file))
                    ) {
                        artifacts = reader.readObject().getJsonArray("artifacts");
                    }
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
