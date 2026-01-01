/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Optional;
import java.util.function.Supplier;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.InputOf;
import org.cactoos.scalar.Unchecked;

/**
 * Take binary files from where {@link MjResolve} placed them and
 * copy to the {@code target/classes} directory.
 *
 * @see <a href="https://news.eolang.org/2022-10-19-placed-catalog.html">Place catalog</a>
 * @since 0.11
 */
@Mojo(
    name = "place",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjPlace extends MjSafe {
    @Override
    public void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(MjResolve.DIR);
        if (Files.exists(home)) {
            final Collection<String> deps = new DepDirs(home);
            final long copied = deps.stream()
                .mapToLong(dep -> this.placeDependency(home, dep))
                .sum();
            if (copied == 0) {
                Logger.info(
                    this, "No binary files placed from %d dependencies into %[file]s",
                    deps.size(), home
                );
            } else {
                Logger.info(
                    this, "Placed %d binary file(s) found in %d dependencies, into %[file]s",
                    copied, deps.size(), home
                );
            }
        } else {
            Logger.info(
                this, "The directory %[file]s is absent, nothing to place from it",
                home
            );
        }
    }

    /**
     * Place one dep.
     * @param home Home to read from
     * @param dep The name of dep
     * @return How many binaries placed
     * @since 0.30
     */
    private long placeDependency(final Path home, final String dep) {
        if (this.placedTojos.findJar(dep).isPresent()) {
            Logger.debug(this, "Found placed binaries from %s", dep);
        }
        final Path dir = home.resolve(dep);
        final long copied = new MjPlace.PlacedDependency(dir, dep, this.rewriteBinaries).get();
        if (copied > 0) {
            Logger.debug(
                this, "Placed %d binary file(s) out of %d, found in %s, to %[file]s",
                copied, new Walk(dir).size(), dep, this.classesDir
            );
        } else {
            Logger.debug(
                this, "No binary file(s) out of %d were placed from %s, to %[file]s",
                new Walk(dir).size(), dep, this.classesDir
            );
        }
        return copied;
    }

    /**
     * Dependency which binaries we are going to place.
     *
     * @since 0.30
     */
    private final class PlacedDependency implements Supplier<Long> {

        /**
         * Directory to read from.
         */
        private final Path dir;

        /**
         * Dependency name.
         */
        private final String dep;

        /**
         * Rewrite binary or not.
         */
        private final boolean rewrite;

        /**
         * Ctor.
         * @param directory The directory to read from
         * @param dependency The name of dependency
         * @param rwte Rewrite binaries in output directory or not
         */
        private PlacedDependency(
            final Path directory,
            final String dependency,
            final boolean rwte
        ) {
            this.dir = directory;
            this.dep = dependency;
            this.rewrite = rwte;
        }

        @Override
        public Long get() {
            return new Walk(this.dir)
                .includes(MjPlace.this.placeBinaries)
                .excludes(MjPlace.this.skipBinaries)
                .stream()
                .filter(this::isNotAlreadyPlaced)
                .peek(this::printLogInfoAboutBinary)
                .peek(this::placeBinary)
                .count();
        }

        /**
         * Check if the file is not already placed.
         * @param file The file to check.
         * @return True if the file is not already placed.
         */
        private boolean isNotAlreadyPlaced(final Path file) {
            final Path target = MjPlace.this.classesDir.toPath().resolve(
                this.dir.relativize(file)
            );
            final Optional<TjPlaced> tojo = MjPlace.this.placedTojos.find(target);
            final boolean res;
            if (tojo.isPresent()
                && Files.exists(target)
                && (this.sameLength(target, file) || !tojo.get().unplaced())
            ) {
                Logger.debug(
                    this,
                    "The same file %[file]s is already placed to %[file]s maybe by %s, skipping",
                    file, target, tojo.get().dependency()
                );
                res = false;
            } else {
                res = true;
            }
            return res;
        }

        /**
         * Print log info about placing class.
         * @param file The file to place.
         */
        private void printLogInfoAboutBinary(final Path file) {
            final Path target = MjPlace.this.classesDir.toPath().resolve(
                this.dir.relativize(file)
            );
            final Optional<TjPlaced> tojo = MjPlace.this.placedTojos.find(target);
            if (tojo.isPresent()) {
                if (!Files.exists(target)) {
                    Logger.info(
                        this,
                        "The file %[file]s has been placed to %[file]s, but now it's gone, replacing",
                        file, target
                    );
                }
                if (Files.exists(target) && !this.sameLength(target, file)) {
                    Logger.debug(
                        this,
                        "File %[file]s (%[size]s) was already placed at %[file]s (%[size]s!) by %s, replacing",
                        file, file.toFile().length(),
                        target, target.toFile().length(),
                        tojo.get().dependency()
                    );
                }
            }
        }

        /**
         * Place class.
         * @param file Absolute path of file to place
         */
        private void placeBinary(final Path file) {
            final Path path = this.dir.relativize(file);
            try {
                final Footprint generated = new FpGenerated(InputOf::new);
                final Path target = new FpIfTargetExists(
                    new FpFork(this.rewrite, generated, new FpIgnore()),
                    generated
                ).apply(file, MjPlace.this.classesDir.toPath().resolve(path));
                MjPlace.this.placedTojos.placeClass(
                    target,
                    MjPlace.this.classesDir.toPath().relativize(target).toString(),
                    this.dep
                );
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    Logger.format(
                        "Failed to place %[file]s to home %[file]s with path %s",
                        file, MjPlace.this.classesDir, path
                    ),
                    ex
                );
            }
        }

        /**
         * Check if two files have the same length.
         * @param first First file
         * @param second Second file
         * @return True if they have the same length
         * @checkstyle NonStaticMethodCheck (2 lines)
         */
        private boolean sameLength(final Path first, final Path second) {
            return new Unchecked<>(() -> Files.size(first) == Files.size(second)).value();
        }
    }
}
