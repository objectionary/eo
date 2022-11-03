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
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.list.ListOf;

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
            final String coords = ResolveMojo.coords(dep);
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
                    coords, new Rel(dest)
                );
                continue;
            }
            this.central.accept(dep, dest);
            final int files = new Walk(dest).size();
            if (files == 0) {
                Logger.warn(
                    this, "No new files after unpacking of %s!",
                    coords
                );
            } else {
                Logger.info(
                    this, "Found %d new file(s) after unpacking of %s",
                    files, coords
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
     * Find all deps for all tojos.
     *
     * @return List of them
     * @throws IOException If fails
     */
    private Collection<Dependency> deps() throws IOException {
        final Collection<Tojo> list = this.scopedTojos().select(
            t -> t.exists(AssembleMojo.ATTR_XMIR)
                && t.exists(AssembleMojo.ATTR_VERSION)
                && !t.exists(AssembleMojo.ATTR_JAR)
        );
        Logger.debug(
            this, "%d suitable tojo(s) found out of %d",
            list.size(), this.scopedTojos().select(t -> true).size()
        );
        final Collection<Dependency> deps = new HashSet<>(0);
        for (final Tojo tojo : list) {
            if (ParseMojo.ZERO.equals(tojo.get(AssembleMojo.ATTR_VERSION))
                && !this.discoverSelf) {
                Logger.debug(
                    this, "Program %s/%s skipped due to its zero version",
                    tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION)
                );
                continue;
            }
            final Optional<Dependency> dep = ResolveMojo.artifact(
                Paths.get(tojo.get(AssembleMojo.ATTR_XMIR))
            );
            if (!dep.isPresent()) {
                Logger.debug(
                    this, "No dependencies for %s/%s",
                    tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION)
                );
                continue;
            }
            final Dependency one = dep.get();
            final String coords = ResolveMojo.coords(one);
            if (this.skipZeroVersions && ParseMojo.ZERO.equals(one.getVersion())) {
                Logger.debug(
                    this, "Zero-version dependency for %s/%s skipped: %s",
                    tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION),
                    coords
                );
                continue;
            }
            Logger.info(
                this, "Dependency found for %s/%s: %s",
                tojo.get(Tojos.KEY), tojo.get(AssembleMojo.ATTR_VERSION), coords
            );
            deps.add(one);
            tojo.set(AssembleMojo.ATTR_JAR, coords);
        }
        return new ListOf<>(this.dependenciesOf(deps)).stream()
            .map(ResolveMojo.Wrap::new)
            .sorted()
            .distinct()
            .map(ResolveMojo.Wrap::dep)
            .collect(Collectors.toList());
    }

    /**
     * Dependencies object.
     *
     * @param all Dependencies as a collection
     * @return Dependencies object with applied decorators.
     */
    private Dependencies dependenciesOf(final Collection<Dependency> all) {
        Dependencies dependencies = new DcsWithRuntime(all::iterator);
        if (!this.ignoreVersionConflicts) {
            dependencies = new DcsWithoutConflicts(dependencies);
        }
        if (!this.ignoreTransitive) {
            dependencies = new DcsNoOneHasTransitive(
                dependencies,
                dep -> new DcsTransitive(
                    new DcsDepgraph(
                        project,
                        session,
                        manager,
                        targetDir.toPath()
                            .resolve(ResolveMojo.DIR)
                            .resolve("dependencies-info"),
                        dep
                    ),
                    dep
                )
            );
        }
        return dependencies;
    }

    /**
     * Find the artifact required by this EO XML.
     *
     * @param file EO file
     * @return List of artifact needed
     * @throws IOException If fails
     */
    private static Optional<Dependency> artifact(final Path file) throws IOException {
        final Collection<String> coords = new XMLDocument(file).xpath(
            "//meta[head='rt' and part[1]='jvm']/part[2]/text()"
        );
        final Optional<Dependency> dep;
        if (coords.isEmpty()) {
            dep = Optional.empty();
        } else if (coords.size() == 1) {
            final String[] parts = coords.iterator().next().split(":");
            final Dependency dependency = new Dependency();
            dependency.setGroupId(parts[0]);
            dependency.setArtifactId(parts[1]);
            if (parts.length == 3) {
                dependency.setVersion(parts[2]);
                dependency.setClassifier("");
            } else {
                dependency.setClassifier(parts[2]);
                dependency.setVersion(parts[3]);
            }
            dependency.setScope("transpile");
            dep = Optional.of(dependency);
        } else {
            throw new IllegalStateException(
                String.format(
                    "Too many (%d) dependencies at %s",
                    coords.size(), new Rel(file)
                )
            );
        }
        return dep;
    }

    /**
     * Dep to coords.
     *
     * @param dep The dependency
     * @return Coords
     */
    private static String coords(final Dependency dep) {
        String ret = "";
        if (dep.getClassifier().isEmpty()) {
            ret = String.format(
                "%s:%s:%s",
                dep.getGroupId(), dep.getArtifactId(), dep.getVersion()
            );
        } else {
            ret = String.format(
                "%s:%s:%s:%s",
                dep.getGroupId(), dep.getArtifactId(), dep.getClassifier(), dep.getVersion()
            );
        }
        return ret;
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
        private Wrap(final Dependency dep) {
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
         *
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
