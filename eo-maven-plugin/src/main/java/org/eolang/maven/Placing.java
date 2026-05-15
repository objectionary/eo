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
import java.util.Set;
import java.util.function.Supplier;
import org.cactoos.io.InputOf;
import org.cactoos.scalar.Unchecked;

/**
 * Core placing logic: copies binary files from the resolved dependency directory
 * into the classes output directory.
 *
 * <p>
 *     Input directory is the resolved-deps folder ({@link MjResolve#DIR}).
 *     Output directory is the classes directory.
 * </p>
 *
 * @see <a href="https://news.eolang.org/2022-10-19-placed-catalog.html">Place catalog</a>
 * @since 0.61.0
 */
final class Placing implements Step {

    /**
     * Catalog of already-placed binaries.
     */
    private final TjsPlaced placed;

    /**
     * Home directory where resolved deps live (targetDir / MjResolve.DIR).
     */
    private final Path home;

    /**
     * Classes output directory.
     */
    private final Path classes;

    /**
     * Inclusion GLOB filters for binary files.
     */
    private final Set<String> include;

    /**
     * Exclusion GLOB filters for binary files.
     */
    private final Set<String> exclude;

    /**
     * Whether to overwrite already-placed binaries.
     */
    private final boolean rewrite;

    /**
     * Constructor.
     * @param placedtojos Catalog of placed binaries
     * @param homedir Home directory of resolved deps
     * @param classesdir Output classes directory
     * @param includebinaries Inclusion GLOB filters
     * @param excludebinaries Exclusion GLOB filters
     * @param rewritebinaries Whether to overwrite
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    Placing(
        final TjsPlaced placedtojos,
        final Path homedir,
        final Path classesdir,
        final Set<String> includebinaries,
        final Set<String> excludebinaries,
        final boolean rewritebinaries
    ) {
        this.placed = placedtojos;
        this.home = homedir;
        this.classes = classesdir;
        this.include = includebinaries;
        this.exclude = excludebinaries;
        this.rewrite = rewritebinaries;
    }

    @Override
    public void exec() throws IOException {
        if (Files.exists(this.home)) {
            final Collection<String> deps = new DepDirs(this.home);
            final long copied = deps.stream()
                .mapToLong(this::placeDependency)
                .sum();
            if (copied == 0) {
                Logger.info(
                    this, "No binary files placed from %d dependencies into %[file]s",
                    deps.size(), this.home
                );
            } else {
                Logger.info(
                    this, "Placed %d binary file(s) found in %d dependencies, into %[file]s",
                    copied, deps.size(), this.home
                );
            }
        } else {
            Logger.info(
                this, "The directory %[file]s is absent, nothing to place from it",
                this.home
            );
        }
    }

    /**
     * Place one dependency.
     * @param dep The name of the dependency
     * @return How many binaries were placed
     */
    private long placeDependency(final String dep) {
        if (this.placed.findJar(dep).isPresent()) {
            Logger.debug(this, "Found placed binaries from %s", dep);
        }
        final Path dir = this.home.resolve(dep);
        final long copied = new Placing.PlacedDependency(dir, dep, this.rewrite).get();
        if (copied > 0) {
            Logger.debug(
                this, "Placed %d binary file(s) out of %d, found in %s, to %[file]s",
                copied, new Walk(dir).size(), dep, this.classes
            );
        } else {
            Logger.debug(
                this, "No binary file(s) out of %d were placed from %s, to %[file]s",
                new Walk(dir).size(), dep, this.classes
            );
        }
        return copied;
    }

    /**
     * Dependency whose binaries are being placed.
     * @since 0.61.0
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
         * Rewrite binaries or not.
         */
        private final boolean rwte;

        /**
         * Ctor.
         * @param directory The directory to read from
         * @param dependency The name of the dependency
         * @param rewrite Rewrite binaries in output directory or not
         */
        private PlacedDependency(
            final Path directory,
            final String dependency,
            final boolean rewrite
        ) {
            this.dir = directory;
            this.dep = dependency;
            this.rwte = rewrite;
        }

        @Override
        public Long get() {
            return new Walk(this.dir)
                .includes(Placing.this.include)
                .excludes(Placing.this.exclude)
                .stream()
                .filter(this::isNotAlreadyPlaced)
                .peek(this::printLogInfoAboutBinary)
                .peek(this::placeBinary)
                .count();
        }

        /**
         * Check if the file has not been placed yet.
         * @param file The file to check
         * @return True if the file is not already placed
         */
        private boolean isNotAlreadyPlaced(final Path file) {
            final Path target = Placing.this.classes.resolve(
                this.dir.relativize(file)
            );
            final Optional<TjPlaced> tojo = Placing.this.placed.find(target);
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
         * Print log info about placing a binary.
         * @param file The file to place
         */
        private void printLogInfoAboutBinary(final Path file) {
            final Path target = Placing.this.classes.resolve(
                this.dir.relativize(file)
            );
            final Optional<TjPlaced> tojo = Placing.this.placed.find(target);
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
         * Place a binary file to the output directory.
         * @param file Absolute path of the file to place
         */
        private void placeBinary(final Path file) {
            final Path path = this.dir.relativize(file);
            try {
                final Footprint generated = new FpGenerated(InputOf::new);
                final Path target = new FpIfTargetExists(
                    new FpFork(this.rwte, generated, new FpIgnore()),
                    generated
                ).apply(file, Placing.this.classes.resolve(path));
                Placing.this.placed.placeClass(
                    target,
                    Placing.this.classes.relativize(target).toString(),
                    this.dep
                );
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    Logger.format(
                        "Failed to place %[file]s to home %[file]s with path %s",
                        file, Placing.this.classes, path
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
