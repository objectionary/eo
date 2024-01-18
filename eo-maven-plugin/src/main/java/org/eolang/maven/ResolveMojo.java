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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Func;
import org.cactoos.list.ListOf;
import org.eolang.maven.dependencies.DcsDefault;
import org.eolang.maven.dependencies.DcsDepgraph;
import org.eolang.maven.dependencies.DcsEachWithoutTransitive;
import org.eolang.maven.dependencies.DcsUniquelyVersioned;
import org.eolang.maven.dependencies.DcsWithRuntime;
import org.eolang.maven.dependencies.DcsWithoutRuntime;
import org.eolang.maven.util.Rel;
import org.eolang.maven.util.Walk;

/**
 * Find all required runtime dependencies, download
 * them from Maven Central, unpack and place to target/eo.
 * The motivation for this mojo is simple: Maven doesn't have
 * a mechanism for adding .JAR files to transpile/test classpath in
 * runtime.
 *
 * @since 0.1
 */
@Mojo(
    name = "resolve",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class ResolveMojo extends SafeMojo {

    /**
     * The directory where to resolve to.
     */
    public static final String DIR = "5-resolve";

    /**
     * Skip artifact with the version 0.0.0.
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.9.0
     */
    @Parameter(property = "eo.skipZeroVersions", required = true, defaultValue = "true")
    private boolean skipZeroVersions;

    /**
     * Shall we discover JAR artifacts for .EO sources?
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.12.0
     */
    @Parameter(property = "eo.discoverSelf", required = true, defaultValue = "false")
    private boolean discoverSelf;

    /**
     * Fail resolution process on conflicting dependencies.
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 1.0
     */
    @Parameter(property = "eo.ignoreVersionConflicts", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.LongVariable")
    private boolean ignoreVersionConflicts;

    /**
     * Fail resolution process on transitive dependencies.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreTransitive", required = true, defaultValue = "false")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean ignoreTransitive;

    /**
     * Add eo-runtime dependency to the classpath.
     *
     * That property is useful only for eo-runtime library compilation.
     * When you compile eo-runtime, you don't want to add eo-runtime from foreign sources
     * (since you compile an eo-runtime library and classpath will anyway have all required classes)
     * and in this case, you should set this property to false.
     * In any other cases, the eo-runtime
     * dependency will be downloaded and added to the classpath automatically.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreRuntime", required = true, defaultValue = "true")
    @SuppressWarnings({"PMD.ImmutableField", "PMD.LongVariable"})
    private boolean withRuntimeDependency = true;

    /**
     * The central.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private BiConsumer<Dependency, Path> central;

    /**
     * Transitive dependency extractor. It's a strategy pattern for extracting transitive
     * dependencies for a particular artifact.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings({"PMD.ImmutableField", "PMD.LongVariable"})
    private Func<Dependency, Iterable<Dependency>> transitiveStrategy =
        dependency -> new DcsDepgraph(
            this.project,
            this.session,
            this.manager,
            this.targetDir.toPath()
                .resolve(ResolveMojo.DIR)
                .resolve("dependencies-info"),
            dependency
        );

    @Override
    public void exec() {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        final Collection<Dependency> deps = this.deps();
        for (final Dependency dep : deps) {
            String classifier = dep.getClassifier();
            if (classifier == null || classifier.isEmpty()) {
                classifier = "-";
            }
            final Path dest = this.targetDir.toPath().resolve(ResolveMojo.DIR)
                .resolve(dep.getGroupId())
                .resolve(dep.getArtifactId())
                .resolve(classifier)
                .resolve(dep.getVersion());
            if (Files.exists(dest)) {
                Logger.debug(
                    this, "Dependency %s already resolved to %s",
                    new Coordinates(dep), new Rel(dest)
                );
                continue;
            }
            this.central.accept(dep, dest);
            final int files = new Walk(dest).size();
            if (files == 0) {
                Logger.warn(
                    this, "No new files after unpacking of %s!",
                    new Coordinates(dep)
                );
            } else {
                Logger.info(
                    this, "Found %d new file(s) after unpacking of %s",
                    files, new Coordinates(dep)
                );
            }
        }
        if (deps.isEmpty()) {
            Logger.debug(this, "No new dependencies unpacked");
        } else {
            Logger.info(this, "New %d dependenc(ies) unpacked", deps.size());
        }
    }

    /**
     * Checks if dependency is runtime.
     * @param dep Dependency
     * @return True if runtime.
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    public static boolean isRuntime(final Dependency dep) {
        return "org.eolang".equals(dep.getGroupId())
            && "eo-runtime".equals(dep.getArtifactId());
    }

    /**
     * Find all deps for all Tojos.
     *
     * @return List of them
     */
    private Collection<Dependency> deps() {
        Iterable<Dependency> deps = new DcsDefault(
            this.scopedTojos(),
            this.discoverSelf,
            this.skipZeroVersions
        );
        if (this.withRuntimeDependency) {
            final Optional<Dependency> runtime = this.runtimeDependencyFromPom();
            if (runtime.isPresent()) {
                deps = new DcsWithRuntime(deps, runtime.get());
                Logger.info(
                    this,
                    "Runtime dependency added from pom with version: %s",
                    runtime.get().getVersion()
                );
            } else {
                deps = new DcsWithRuntime(deps);
            }
        } else {
            deps = new DcsWithoutRuntime(deps);
        }
        if (!this.withVersions && !this.ignoreVersionConflicts) {
            deps = new DcsUniquelyVersioned(deps);
        }
        if (!this.ignoreTransitive) {
            deps = new DcsEachWithoutTransitive(deps, this.transitiveStrategy);
        }
        return new ListOf<>(deps)
            .stream()
            .map(ResolveMojo.Wrap::new)
            .sorted()
            .distinct()
            .map(ResolveMojo.Wrap::dep)
            .collect(Collectors.toList());
    }

    /**
     * Runtime dependency from pom.xml.
     *
     * @return Dependency if found.
     */
    private Optional<Dependency> runtimeDependencyFromPom() {
        final Optional<Dependency> res;
        if (this.project == null) {
            res = Optional.empty();
        } else {
            res = this.project
                .getDependencies()
                .stream()
                .filter(ResolveMojo::isRuntime)
                .findFirst();
        }
        return res;
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
         *
         * @param dep Dependency
         */
        Wrap(final Dependency dep) {
            this.dependency = dep;
        }

        /**
         * Return it.
         *
         * @return The dep
         */
        public Dependency dep() {
            return this.dependency;
        }

        @Override
        public int compareTo(final ResolveMojo.Wrap wrap) {
            return new Coordinates(this.dependency).compareTo(
                new Coordinates(wrap.dependency)
            );
        }

        @Override
        public boolean equals(final Object wrap) {
            return new Coordinates(this.dependency).equals(
                new Coordinates(
                    ResolveMojo.Wrap.class.cast(wrap).dependency
                )
            );
        }

        @Override
        public int hashCode() {
            return new Coordinates(this.dependency).hashCode();
        }
    }
}
