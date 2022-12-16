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
import java.util.Collection;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.eolang.maven.util.Rel;
import org.eolang.maven.util.Walk;

/**
 * Find all required runtime dependencies, download
 * them from Maven Central, unpack and place to target/eo.
 * The motivation for this mojo is simple: Maven doesn't have
 * a mechanism of adding .JAR files to transpile/test classpath in
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
    public static final String DIR = "06-resolve";

    /**
     * Skip artifact with the version 0.0.0.
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.9.0
     */
    @Parameter(property = "eo.skipZeroVersions", required = true, defaultValue = "true")
    private Boolean skipZeroVersions;

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
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.ignoreRuntime", required = true, defaultValue = "true")
    @SuppressWarnings("PMD.ImmutableField")
    private boolean withRuntimeDependency = true;

    /**
     * The central.
     */
    @SuppressWarnings("PMD.ImmutableField")
    private BiConsumer<Dependency, Path> central;

    @Override
    public void exec() throws IOException {
        if (this.central == null) {
            this.central = new Central(this.project, this.session, this.manager);
        }
        final Collection<Dependency> deps = this.deps();
        for (final Dependency dep : deps) {
            String classifier = dep.getClassifier();
            if (classifier.isEmpty()) {
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
            deps = new DcsWithRuntime(deps);
        }
        if (!this.ignoreVersionConflicts) {
            deps = new DcsUniquelyVersioned(deps);
        }
        if (!this.ignoreTransitive) {
            deps = new Mapped<>(
                dependency -> {
                    final Iterable<Dependency> transitives = new Filtered<>(
                        dep -> !ResolveMojo.eqTo(dep, dependency)
                            && !dep.getScope().contains("test")
                            && !("org.eolang".equals(dep.getGroupId())
                                     && "eo-runtime".equals(dep.getArtifactId())),
                        new DcsDepgraph(
                            this.project,
                            this.session,
                            this.manager,
                            this.targetDir.toPath()
                                .resolve(ResolveMojo.DIR)
                                .resolve("dependencies-info"),
                            dependency
                        )
                    );
                    final String list = String.join(
                        ", ",
                        new Mapped<>(
                            dep -> new Coordinates(dep).toString(),
                            transitives
                        )
                    );
                    if (!list.isEmpty()) {
                        throw new IllegalStateException(
                            String.format(
                                "%s contains transitive dependencies: [%s]",
                                dependency, list
                            )
                        );
                    }
                    return dependency;
                },
                deps
            );
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
     * Compare with NULL-safety.
     * @param left Left
     * @param right Right
     * @return TRUE if they are equal
     */
    private static boolean eqTo(final Dependency left, final Dependency right) {
        return Objects.equals(
            Objects.toString(left.getClassifier(), ""),
            Objects.toString(right.getClassifier(), "")
        )
            && Objects.equals(left.getArtifactId(), right.getArtifactId())
            && Objects.equals(left.getGroupId(), right.getGroupId());
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
