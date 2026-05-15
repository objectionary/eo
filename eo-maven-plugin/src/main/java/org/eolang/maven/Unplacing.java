/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Set;

/**
 * Core unplacing logic: removes binary files that were previously placed by {@link Placing}.
 *
 * <p>
 *     Goes through all entries in the placed-binaries catalog, compares file hashes,
 *     and deletes files that are still in the same state as when they were placed.
 * </p>
 *
 * @since 0.61.0
 * @checkstyle ExecutableStatementCountCheck (500 lines)
 */
final class Unplacing implements Step {

    /**
     * Catalog of placed binaries.
     */
    private final TjsPlaced placed;

    /**
     * Classes output directory.
     */
    private final Path classes;

    /**
     * Binaries to keep (GLOB filters).
     */
    private final Set<String> keep;

    /**
     * Constructor.
     * @param plcd Placed binaries catalog
     * @param clsses Classes output directory
     * @param keepbinaries Binaries to keep
     */
    Unplacing(
        final TjsPlaced plcd,
        final Path clsses,
        final Set<String> keepbinaries
    ) {
        this.placed = plcd;
        this.classes = clsses;
        this.keep = keepbinaries;
    }

    @Override
    public void exec() throws IOException {
        if (this.placed.isEmpty()) {
            Logger.info(this, "The list of placed binaries is empty, nothing to unplace");
        } else {
            this.unplace();
        }
    }

    /**
     * Unplace what's necessary.
     */
    private void unplace() {
        final Walk binaries = new Walk(this.classes);
        if (binaries.isEmpty()) {
            Logger.warn(this, "No classes found in %[file]s", this.classes);
        } else {
            final Collection<Path> available = binaries.excludes(this.keep);
            final int total = new Threaded<>(
                this.placed.classes(),
                tojo -> this.unplace(tojo, available)
            ).total();
            new EmptyDirectoriesIn(this.classes).clear();
            if (total == 0) {
                Logger.info(
                    this, "No binaries out of %d deleted in %[file]s",
                    binaries.size(), this.classes
                );
            } else if (total == available.size()) {
                Logger.info(
                    this, "All %d binari(es) deleted, which were found in %[file]s",
                    binaries.size(), this.classes
                );
            } else {
                Logger.info(
                    this, "Just %d binari(es) out of %d deleted in %[file]s",
                    total, binaries.size(), this.classes
                );
            }
        }
    }

    /**
     * Unplace a single tojo.
     * @param tojo Placed tojo
     * @param available All available classes
     * @return Amount of unplaced binaries
     * @throws IOException If fails to unplace
     */
    private int unplace(
        final TjPlaced tojo,
        final Collection<Path> available
    ) throws IOException {
        final String related = tojo.related();
        final Path path = Paths.get(tojo.identifier());
        final String hash = new FileHash(path).toString();
        final int total;
        final boolean inside = available.stream().anyMatch(path::equals);
        if (tojo.sameHash(hash)) {
            if (inside) {
                Logger.debug(
                    this, "The binary %s of %s looks the same, so it's unplaced",
                    related, tojo.dependency()
                );
                total = Unplacing.unplaced(tojo, path);
            } else {
                Logger.debug(
                    this, "The binary %s of %s looks the same, but can't be unplaced",
                    related, tojo.dependency()
                );
                total = 0;
            }
        } else {
            if (hash.isEmpty()) {
                Logger.debug(
                    this, "The binary %s of %s is gone, won't unplace",
                    related, tojo.dependency()
                );
                total = 0;
            } else if (inside) {
                Logger.debug(
                    this,
                    "The binary %s of %s looks different, but its unplacing is mandatory",
                    related, tojo.dependency()
                );
                total = Unplacing.unplaced(tojo, path);
            } else {
                Logger.debug(
                    this,
                    "The binary %s of %s looks different, but can't be unplaced",
                    related, tojo.dependency()
                );
                total = 0;
            }
        }
        return total;
    }

    /**
     * Mark tojo as unplaced and delete the binary file.
     * @param tojo Placed tojo
     * @param path Path to binary
     * @return Amount of unplaced binaries (always 1)
     * @throws IOException If fails to delete binary
     */
    private static int unplaced(final TjPlaced tojo, final Path path) throws IOException {
        tojo.unplace();
        if (Files.deleteIfExists(path)) {
            Logger.debug(Unplacing.class, "Deleted binary %s", path);
        }
        return 1;
    }
}
