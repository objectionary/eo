/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
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

    @Test
    void sortsNamesOfDirectChildren(@TempDir final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final Random random = new Random(seed);
        final List<String> names = new ArrayList<>(0);
        for (int index = 0; index < 8; ++index) {
            final String name = String.format("файл %04d %d ~", random.nextInt(10_000), index);
            Files.write(temp.resolve(name), new byte[0]);
            names.add(name);
        }
        Collections.sort(names);
        MatcherAssert.assertThat(
            String.format("listed names are not the children in sorted order, seed is %d", seed),
            this.names(temp),
            Matchers.equalTo(names)
        );
    }

    @Test
    void skipsGrandchildrenOfTheDirectory(@TempDir final Path temp) throws IOException {
        Files.write(
            Files.createDirectory(temp.resolve("вложенная")).resolve("глубина"),
            new byte[0]
        );
        MatcherAssert.assertThat(
            "listed names reach deeper than the direct children of the directory",
            this.names(temp),
            Matchers.equalTo(Collections.singletonList("вложенная"))
        );
    }

    /**
     * The names the atom lists for the directory at this path.
     * @param path The path of the directory to list
     * @return The listed names, in the order the atom returns them
     */
    private List<String> names(final Path path) {
        final List<String> names = new ArrayList<>(0);
        for (final Phi name : new TupleToArray(this.listing(path.toString())).get()) {
            names.add(new Dataized(name).asString());
        }
        return names;
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
