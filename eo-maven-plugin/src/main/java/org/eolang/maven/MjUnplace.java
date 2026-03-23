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
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * It deletes binary files, which were previously copied by "place" mojo so
 * these binaries are not got into result JAR.
 *
 * @since 0.11
 * @checkstyle ExecutableStatementCountCheck (500 lines)
 */
@Mojo(
    name = "unplace",
    defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
    threadSafe = true
)
public final class MjUnplace extends MjSafe {
    @Override
    public void exec() throws IOException {
        if (this.placedTojos.isEmpty()) {
            Logger.info(this, "The list of placed binaries is absent: %[file]s", this.placed);
        } else {
            this.unplace();
        }
    }

    /**
     * Unplace what's necessary.
     */
    private void unplace() {
        final Path classes = this.classesDir.toPath();
        final Walk binaries = new Walk(classes);
        if (binaries.isEmpty()) {
            Logger.warn(this, "No classes found in %[file]s", classes);
        } else {
            final Collection<Path> available = binaries.excludes(this.keepBinaries);
            final int unplaced = new Threaded<>(
                this.placedTojos.classes(),
                tojo -> this.unplace(tojo, available)
            ).total();
            new EmptyDirectoriesIn(classes).clear();
            if (unplaced == 0) {
                Logger.info(
                    this, "No binaries out of %d deleted in %[file]s",
                    binaries.size(), this.placed
                );
            } else if (unplaced == available.size()) {
                Logger.info(
                    this, "All %d binari(es) deleted, which were found in %[file]s",
                    binaries.size(), this.placed
                );
            } else {
                Logger.info(
                    this, "Just %d binari(es) out of %d deleted in %[file]s",
                    unplaced, binaries.size(), this.placed
                );
            }
        }
    }

    /**
     * Unplace provided tojo.
     * @param tojo Placed tojo
     * @param classes All available classes
     * @return Amount of unplaced binaries
     * @throws IOException If fails to unplace
     */
    private int unplace(final TjPlaced tojo, final Collection<Path> classes) throws IOException {
        final String related = tojo.related();
        final Path path = Paths.get(tojo.identifier());
        final String hash = new FileHash(path).toString();
        final int unplaced;
        final boolean inside = classes.stream().anyMatch(path::equals);
        if (tojo.sameHash(hash)) {
            if (inside) {
                Logger.debug(
                    this, "The binary %s of %s looks the same, so it's unplaced",
                    related, tojo.dependency()
                );
                unplaced = MjUnplace.unplaced(tojo, path);
            } else {
                Logger.debug(
                    this, "The binary %s of %s looks the same, but can't be unplaced",
                    related, tojo.dependency()
                );
                unplaced = 0;
            }
        } else {
            if (hash.isEmpty()) {
                Logger.debug(
                    this, "The binary %s of %s is gone, won't unplace",
                    related, tojo.dependency()
                );
                unplaced = 0;
            } else if (inside) {
                Logger.debug(
                    this,
                    "The binary %s of %s looks different, but its unplacing is mandatory",
                    related, tojo.dependency()
                );
                unplaced = MjUnplace.unplaced(tojo, path);
            } else {
                Logger.debug(
                    this,
                    "The binary %s of %s looks different, but can't be unplaced",
                    related, tojo.dependency()
                );
                unplaced = 0;
            }
        }
        return unplaced;
    }

    /**
     * Unplaced and deleted binary.
     * @param tojo Placed tojo
     * @param path Path to binary
     * @return Amount of unplaced binaries
     * @throws IOException If fails to delete binary
     */
    private static int unplaced(final TjPlaced tojo, final Path path) throws IOException {
        tojo.unplace();
        if (Files.deleteIfExists(path)) {
            Logger.debug(MjUnplace.class, "Deleted binary %s", path);
        }
        return 1;
    }
}
