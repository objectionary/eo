/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.Func;
import org.cactoos.iterable.Mapped;
import org.cactoos.set.SetOf;
import org.cactoos.text.Joined;

/**
 * Find all required runtime dependencies, download
 * them from Maven Central, unpack and place to the {@code target/eo}
 * directory.
 *
 * <p>The motivation for this mojo is simple: Maven doesn't have
 * a mechanism for adding .JAR files to transpile/test classpath in
 * runtime.</p>
 *
 * @since 0.1
 */
@Mojo(
    name = "resolve",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjResolve extends MjSafe {

    /**
     * The directory where to resolve to.
     */
    static final String DIR = "4-resolve";

    /**
     * Transitive dependency extractor. It's a strategy pattern for extracting transitive
     * dependencies for a particular artifact.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings({"PMD.ImmutableField", "PMD.LongVariable"})
    private Func<Dep, Dependencies> transitiveStrategy = dependency -> new DpsDepgraph(
        this.project,
        this.session,
        this.manager,
        this.targetDir.toPath().resolve(MjResolve.DIR).resolve("dependencies-info"),
        dependency
    );

    /**
     * Resolve default JNA dependency or not.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private boolean resolveJna = true;

    /**
     * Resolve dependencies in central or not.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    private boolean resolveInCentral = true;

    @Override
    public void exec() throws IOException {
        final Collection<Dep> deps = this.deps();
        if (deps.isEmpty()) {
            Logger.info(this, "No new dependencies unpacked");
        } else {
            final Path target = this.targetDir.toPath().resolve(MjResolve.DIR);
            new Threaded<>(
                deps,
                dep -> this.resolved(dep, target)
            ).total();
            Logger.info(
                this,
                "New %d dependenc(ies) unpacked to %[file]s: %s",
                deps.size(), target,
                new Joined(", ", new Mapped<>(Dep::toString, deps))
            );
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
     * Resolved dependency.
     * @param dep Dependency
     * @param target Target path
     * @return Amount of resolved dependencies
     * @throws IOException If fails to resolve
     */
    private int resolved(final Dep dep, final Path target) throws IOException {
        final String classifier;
        final Dependency dependency = dep.get();
        if (dependency.getClassifier() == null
            || dependency.getClassifier().isEmpty()) {
            classifier = "-";
        } else {
            classifier = dependency.getClassifier();
        }
        final Path dest = this.cleanPlace(
            target
                .resolve(dependency.getGroupId())
                .resolve(dependency.getArtifactId())
                .resolve(classifier),
            dependency.getVersion()
        );
        final int total;
        if (Files.exists(dest)) {
            Logger.debug(
                this,
                "Dependency %s already resolved and exists in %[file]s",
                dep, dest
            );
            total = 0;
        } else {
            this.central.accept(dependency, dest);
            final int files = new Walk(dest).size();
            if (files == 0) {
                Logger.warn(this, "No new files after unpacking of %s!", dep);
            } else {
                Logger.info(
                    this, "Found %d new file(s) (%d MB) after unpacking of %s",
                    files, MjResolve.folderSizeInMb(dest), dep
                );
            }
            total = 1;
        }
        return total;
    }

    /**
     * Returns directory where the files should be unpacked,
     * making sure there are no old files around.
     *
     * <p>Say, on the first run of "resolve" binary files from some JAR
     * get unpacked. Then, the user changes the version of the JAR
     * in the "pom.xml" (or in some .eo file) and runs the build again.
     * We don't want the old binary files to stay in the "5-resolve"
     * directory, because they are outdated. We want them to be removed
     * and only the new files from the right version of the JAR to be there.
     * This method does exactly this: it removes old files and doesn't
     * touches new ones.</p>
     *
     * @param dir The dir
     * @param version Version of dependency
     * @return Full path
     * @throws IOException If fails
     */
    private Path cleanPlace(final Path dir, final String version) throws IOException {
        final File[] subs = dir.toFile().listFiles();
        if (subs != null) {
            for (final File sub : subs) {
                final String base = sub.getName();
                Logger.info(this, "Base if %s", base);
                if (base.equals(version)) {
                    continue;
                }
                final Path bad = dir.resolve(base);
                try (Stream<Path> walk = Files.walk(bad)) {
                    walk
                        .map(Path::toFile)
                        .sorted(Comparator.reverseOrder())
                        .forEach(File::delete);
                }
                Logger.info(
                    this,
                    "Directory %[file]s deleted because it contained wrong version files (not %s)",
                    bad, version
                );
            }
        }
        return dir.resolve(version);
    }

    /**
     * Find all deps for all Tojos.
     *
     * @return List of them
     */
    private Collection<Dep> deps() {
        Dependencies deps = new DpsDefault(
            this.scopedTojos(), this.discoverSelf, this.skipZeroVersions, this.resolveJna
        );
        if (this.ignoreRuntime) {
            Logger.info(this, "Runtime dependency is ignored because eo:ignoreRuntime=TRUE");
            deps = new DpsWithoutRuntime(deps);
        } else {
            final Optional<Dependency> runtime = this.runtimeDependencyFromPom();
            if (runtime.isPresent()) {
                deps = new DpsWithRuntime(deps, new Dep(runtime.get()));
                Logger.info(
                    this,
                    "Runtime dependency added from pom with version: %s",
                    runtime.get().getVersion()
                );
            } else {
                if (this.resolveInCentral) {
                    deps = new DpsWithRuntime(deps);
                } else {
                    deps = new DpsOfflineRuntime(deps);
                }
            }
        }
        if (!this.ignoreVersionConflicts) {
            deps = new DpsUniquelyVersioned(deps);
        }
        if (!this.ignoreTransitive) {
            deps = new DpsEachWithoutTransitive(deps, this.transitiveStrategy);
        }
        return new SetOf<>(deps)
            .stream()
            .sorted()
            .distinct()
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
                .filter(MjResolve::isRuntime)
                .findFirst();
        }
        return res;
    }

    /**
     * Folder size in megabytes.
     * @param path Folder
     * @return Folder size
     * @throws IOException if I/O fails
     */
    private static long folderSizeInMb(final Path path) throws IOException {
        try (Stream<Path> paths = Files.walk(path)) {
            return paths.filter(Files::isRegularFile).mapToLong(
                p -> {
                    try {
                        return Files.size(p);
                    } catch (final IOException exception) {
                        throw new IllegalStateException(
                            String.format("Failed to calculate size in %s", p),
                            exception
                        );
                    }
                }
            ).sum() / 1024L / 1024L;
        }
    }
}
