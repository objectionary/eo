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
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.maven.model.Dependency;
import org.cactoos.Scalar;
import org.cactoos.iterable.Mapped;
import org.cactoos.set.SetOf;
import org.cactoos.text.Joined;

/**
 * Resolves all required runtime dependencies: downloads from Maven Central,
 * unpacks and places them into the target directory.
 *
 * @since 0.61.0
 */
final class Resolving implements Step {

    /**
     * Tojos.
     */
    private final TjsForeign tojos;

    /**
     * Target directory.
     */
    private final Path target;

    /**
     * Central dependency consumer.
     */
    private final BiConsumer<Dependency, Path> central;

    /**
     * Discover self too.
     */
    private final boolean discover;

    /**
     * Skip zero versions.
     */
    private final boolean skipzero;

    /**
     * Resolve default JNA dependency.
     */
    private final boolean jna;

    /**
     * Ignore runtime dependency.
     */
    private final boolean noruntime;

    /**
     * Maven runtime dependency supplier.
     */
    private final Scalar<Dep> runtime;

    /**
     * Ignore version conflicts.
     */
    private final boolean noconflicts;

    /**
     * Locks serializing {@link #cleanPlace(Path, String, Set)} per directory,
     * since sibling versions of a dependency share a directory and are
     * cleaned up concurrently by {@link Threaded}.
     */
    private final ConcurrentMap<Path, ReentrantLock> locks;

    /**
     * Ctor.
     *
     * @param tjs Tojos
     * @param tgt Target directory
     * @param cntrl Central dependency consumer
     * @param self Discover self
     * @param zero Skip zero versions
     * @param jnadep Resolve default JNA
     * @param norun Ignore runtime
     * @param runtime EO runtime dependency supplier
     * @param noconf Ignore version conflicts
     */
    Resolving(
        final TjsForeign tjs,
        final Path tgt,
        final BiConsumer<Dependency, Path> cntrl,
        final boolean self,
        final boolean zero,
        final boolean jnadep,
        final boolean norun,
        final Scalar<Dep> runtime,
        final boolean noconf
    ) {
        this.tojos = tjs;
        this.target = tgt;
        this.central = cntrl;
        this.discover = self;
        this.skipzero = zero;
        this.jna = jnadep;
        this.noruntime = norun;
        this.runtime = runtime;
        this.noconflicts = noconf;
        this.locks = new ConcurrentHashMap<>(0);
    }

    @Override
    public void exec() {
        final Collection<Dep> deps = this.deps();
        final int unpacked;
        if (deps.isEmpty()) {
            unpacked = 0;
        } else {
            final Map<String, Set<String>> versions = new ResolvedVersions(deps).byCoordinate();
            unpacked = new Threaded<>(
                deps,
                dep -> this.resolved(dep, this.target, versions)
            ).total();
        }
        if (unpacked == 0) {
            Logger.info(this, "No new dependencies unpacked");
        } else {
            Logger.info(
                this,
                "New %d dependenc(ies) unpacked to %[file]s: %s",
                unpacked, this.target,
                new Joined(", ", new Mapped<>(Dep::toString, deps))
            );
        }
    }

    /**
     * Delete every stale sibling version found in the given directory.
     *
     * @param dir The directory shared by every version of one dependency
     * @param version The version being resolved
     * @param keep The versions to keep, everything else in {@code dir} is stale
     * @return The place of the version being resolved
     * @throws IOException If fails to delete a stale version
     */
    Path cleanPlace(
        final Path dir, final String version, final Set<String> keep
    ) throws IOException {
        final ReentrantLock lock = this.locks.computeIfAbsent(
            dir.normalize(), key -> new ReentrantLock()
        );
        lock.lock();
        try {
            final File[] subs = dir.toFile().listFiles();
            if (subs != null) {
                for (final File sub : subs) {
                    final String base = sub.getName();
                    if (keep.contains(base)) {
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
                        "Directory %[file]s deleted because it contained a stale version (not %s)",
                        bad, keep
                    );
                }
            }
        } finally {
            lock.unlock();
        }
        return dir.resolve(version);
    }

    private int resolved(
        final Dep dep, final Path dest, final Map<String, Set<String>> versions
    ) throws IOException {
        final Dependency dependency = dep.get();
        final DepCoordinate coords = new DepCoordinate(dependency);
        final Path place = this.cleanPlace(
            dest
                .resolve(dependency.getGroupId())
                .resolve(dependency.getArtifactId())
                .resolve(coords.classifier()),
            dependency.getVersion(),
            versions.get(coords.value())
        );
        final int total;
        if (Files.exists(place)) {
            if (new WkDefault(place).isEmpty()) {
                Logger.debug(
                    this,
                    "Destination %[file]s exists but is empty, unpacking %s again",
                    place, dep
                );
                total = this.unpacked(dep, dependency, place);
            } else {
                Logger.debug(
                    this,
                    "Dependency %s already resolved and exists in %[file]s",
                    dep, place
                );
                total = 0;
            }
        } else {
            total = this.unpacked(dep, dependency, place);
        }
        return total;
    }

    private int unpacked(
        final Dep dep, final Dependency dependency, final Path place
    ) throws IOException {
        this.central.accept(dependency, place);
        final int files = new WkDefault(place).size();
        if (files == 0) {
            Logger.warn(this, "No new files after unpacking of %s!", dep);
        } else {
            Logger.info(
                this, "Found %d new file(s) (%d MB) after unpacking of %s",
                files, Resolving.folderSizeInMb(place), dep
            );
        }
        return 1;
    }

    private Collection<Dep> deps() {
        Dependencies result = new DpsDefault(
            this.tojos, this.discover, this.skipzero, this.jna
        );
        if (this.noruntime) {
            Logger.info(this, "Runtime dependency is ignored because eo:ignoreRuntime=TRUE");
            result = new DpsWithoutRuntime(result);
        } else {
            result = new DpsWithRuntime(result, this.runtime);
        }
        if (!this.noconflicts) {
            result = new DpsUniquelyVersioned(result);
        }
        return new SetOf<>(result)
            .stream()
            .sorted()
            .distinct()
            .collect(Collectors.toList());
    }

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
