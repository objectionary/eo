/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import org.cactoos.io.Directory;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Sorted;

/**
 * The list of the copies of the sources, which have no tests inside.
 *
 * <p>The stage {@link Pruning} makes a copy of every source file of the
 * build, removes the tests from it, and saves it into the directory
 * {@code 1-planting}, inside the home directory of the lowering. All the
 * stages after {@link Pruning} read only these copies, and never the
 * original sources. This class lists those copies.</p>
 *
 * <p>The copies are always listed in the order of their file names. The
 * build may find the source files in any order, but thanks to this sorting
 * the entries and the world are exactly the same on every build of the
 * same program.</p>
 *
 * @since 0.64.0
 */
final class Copies implements Iterable<Path> {

    /**
     * The home directory of the lowering, where the directory of copies is.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The home directory of the lowering, where the directory of copies is
     */
    Copies(final Path dir) {
        this.home = dir;
    }

    @Override
    public Iterator<Path> iterator() {
        return new Sorted<>(
            new Filtered<>(
                Files::isRegularFile,
                new Directory(this.home.resolve("1-planting"))
            )
        ).iterator();
    }
}
