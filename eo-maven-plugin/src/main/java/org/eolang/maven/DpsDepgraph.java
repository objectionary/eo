/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
 */
final class DpsDepgraph implements Dependencies {

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
    private final Dep dependency;

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
    DpsDepgraph(
        final MavenProject pkt,
        final MavenSession ssn,
        final BuildPluginManager mgr,
        final Path path,
        final Dep dep
    ) {
        this.project = pkt;
        this.session = ssn;
        this.manager = mgr;
        this.dir = path;
        this.dependency = dep;
    }

    @Override
    public Iterator<Dep> iterator() {
        return new DpsJson(this.file(this.dependency)).iterator();
    }

    /**
     * Receive file with dependencies jar file.
     *
     * @param dep Dependency
     * @return Path to the saved json dependency file
     */
    private Path file(final Dep dep) {
        try {
            final Dependency dpndncy = dep.get();
            final String name = DpsDepgraph.fileName(dpndncy);
            MojoExecutor.executeMojo(
                MojoExecutor.plugin(
                    MojoExecutor.groupId("com.github.ferstl"),
                    MojoExecutor.artifactId("depgraph-maven-plugin"),
                    MojoExecutor.version("4.0.2")
                ),
                MojoExecutor.goal("for-artifact"),
                MojoExecutor.configuration(
                    MojoExecutor.element("groupId", dpndncy.getGroupId()),
                    MojoExecutor.element("artifactId", dpndncy.getArtifactId()),
                    MojoExecutor.element("version", dpndncy.getVersion()),
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
                    dep
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
    static final class DpsJson implements Dependencies {

        /**
         * File path.
         */
        private final Path file;

        /**
         * The main constructor.
         *
         * @param path File path
         */
        DpsJson(final Path path) {
            this.file = path;
        }

        @Override
        public Iterator<Dep> iterator() {
            try {
                final Collection<Dep> all = new ArrayList<>(0);
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
                        all.add(
                            new Dep(new Dependency())
                                .withGroupId(obj.getString("groupId"))
                                .withArtifactId(obj.getString("artifactId"))
                                .withVersion(obj.getString("version"))
                                .withScope(
                                    obj.getJsonArray("scopes")
                                        .stream()
                                        .map(JsonValue::toString)
                                        .findFirst()
                                        .orElseThrow(IllegalStateException::new)
                                )
                        );
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
