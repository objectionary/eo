/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link EOdirectory$EOlisted}.
 * @since 0.63
 */
final class EOdirectoryEOlistedTest {

    @Test
    void wrapsInvalidPathIntoExFailure(@TempDir final Path temp) {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                this.listing(String.format("%s%cпапка", temp, (char) 0))
            ).take(),
            "a path with a NUL character must fail with ExFailure, not a raw InvalidPathException"
        );
    }

    @Test
    void includesInvalidPathReason(@TempDir final Path temp) {
        final ExFailure failure = Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                this.listing(String.format("%s%cпапка", temp, (char) 0))
            ).take()
        );
        MatcherAssert.assertThat(
            "the failure does not say why the path could not be parsed",
            failure.getMessage(),
            Matchers.containsString(
                ((InvalidPathException) failure.getCause()).getReason()
            )
        );
    }

    @Test
    void wrapsMissingDirectoryIntoExFailure(@TempDir final Path temp) {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                this.listing(temp.resolve("отсутствует").toString())
            ).take(),
            "listing a directory that is not there must fail with ExFailure, not a raw IOException"
        );
    }

    /**
     * Make an application of the atom to a directory at this path.
     * @param path The path of the directory to list
     * @return The atom applied to the directory
     */
    private Phi listing(final String path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        return new PhApplication(new EOdirectory$EOlisted(), Phi.RHO, directory);
    }
}
