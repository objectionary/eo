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
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Mapped;
import org.eolang.tojos.MonoTojos;
import org.eolang.tojos.Tojo;
import org.eolang.tojos.Tojos;
import org.twdata.maven.mojoexecutor.MojoExecutor;

/**
 * Find all required runtime dependencies and add
 * them to classpath ("transpile" scope).
 *
 * The motivation for this mojo is simple: Maven doesn't have
 * a mechanism of adding .JAR files to transpile/test classpath in
 * runtime.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@Mojo(
    name = "resolve",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class ResolveMojo extends SafeMojo {

    /**
     * Output.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.outputDirectory}"
    )
    private File outputDir;

    /**
     * The path to a text file where paths of all added
     * .class (and maybe others) files are placed.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.11.0
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo-resolved.csv"
    )
    private File resolvedList;

    /**
     * Skip artifact with the version 0.0.0.
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.9.0
     */
    @Parameter(required = true, defaultValue = "true")
    private boolean skipZeroVersions;

    /**
     * Overwrite existing .class files?
     * @checkstyle MemberNameCheck (7 lines)
     * @since 0.10.0
     */
    @Parameter(required = true, defaultValue = "true")
    private Boolean overWrite;

    @Override
    @SuppressWarnings({ "PMD.GuardLogStatement", "PMD.PrematureDeclaration" })
    public void exec() throws IOException {
        final Collection<Path> added = new HashSet<>(0);
        for (final Dependency dep : this.deps()) {
            final List<Path> before = this.files();
            try {
                this.unpack(dep);
            } catch (final MojoExecutionException ex) {
                throw new IllegalStateException(ex);
            }
            final List<Path> after = this.files();
            if (before.size() < after.size()) {
                Logger.info(
                    this, "%d new file(s) after unpacking of %s:%s:%s",
                    after.size() - before.size(),
                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion()
                );
            } else {
                Logger.warn(
                    this, "No new files after unpacking of %s:%s:%s!",
                    dep.getGroupId(), dep.getArtifactId(), dep.getVersion()
                );
            }
            after.removeAll(before);
            added.addAll(after);
        }
        new Save(
            String.join("\n", new Mapped<>(Path::toString, added)),
            this.resolvedList.toPath()
        ).save();
    }

    /**
     * Find all deps for all tojos.
     *
     * @return List of them
     * @throws IOException If fails
     */
    private Collection<Dependency> deps() throws IOException {
        final Tojos tojos = new MonoTojos(this.foreign);
        final Collection<Tojo> list = tojos.select(
            t -> t.exists(AssembleMojo.ATTR_XMIR)
                && !t.exists("jar")
                && !ParseMojo.ZERO.equals(t.get(AssembleMojo.ATTR_VERSION))
        );
        final Collection<Dependency> deps = new HashSet<>(0);
        for (final Tojo tojo : list) {
            final Optional<Dependency> dep = ResolveMojo.artifact(
                Paths.get(tojo.get(AssembleMojo.ATTR_XMIR))
            );
            if (!dep.isPresent()) {
                continue;
            }
            final Dependency one = dep.get();
            deps.add(one);
            tojo.set(
                "dependency",
                String.format(
                    "%s:%s:%s",
                    one.getGroupId(), one.getArtifactId(), one.getVersion()
                )
            );
            this.jarSources(tojos, one.getVersion());
        }
        return deps.stream()
            .filter(dep -> !this.skipZeroVersions || !"0.0.0".equals(dep.getVersion()))
            .map(ResolveMojo.Wrap::new)
            .sorted()
            .distinct()
            .map(ResolveMojo.Wrap::dep)
            .collect(Collectors.toList());
    }

    /**
     * Take sources from EO-SOURCES dir and register them in the CSV.
     *
     * @param tojos The tojos
     * @param version The version of the JAR
     * @throws IOException If fails
     */
    private void jarSources(final Tojos tojos, final String version) throws IOException {
        final Path home = this.outputDir.toPath().resolve(CopyMojo.DIR);
        final Unplace unplace = new Unplace(home);
        for (final Path src : new Walk(home)) {
            if (src.endsWith(".eo")) {
                tojos.add(unplace.make(src)).set(AssembleMojo.ATTR_VERSION, version);
            }
            Files.delete(src);
        }
    }

    /**
     * How many files in the target dir?
     *
     * @return Total count
     * @throws IOException If fails
     */
    private List<Path> files() throws IOException {
        return new Walk(this.outputDir.toPath());
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
                        MojoExecutor.element("overWrite", this.overWrite.toString()),
                        MojoExecutor.element(
                            "outputDirectory",
                            this.outputDir.toString()
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
            this.outputDir
        );
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
            dependency.setVersion(parts[2]);
            dependency.setClassifier("");
            dependency.setScope("transpile");
            dep = Optional.of(dependency);
        } else {
            throw new IllegalStateException(
                String.format(
                    "Too many (%d) dependencies at %s",
                    coords.size(), file
                )
            );
        }
        return dep;
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
