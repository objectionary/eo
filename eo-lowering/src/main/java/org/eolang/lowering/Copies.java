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
 * The copies of the sources the pruning wrote, with their tests cut out.
 *
 * <p>The stages after the pruning never see the sources of the build, only
 * these copies, and they read them in the order of their names, so the
 * entries and the world come out the same on every run however the files
 * of the build were listed.</p>
 *
 * @since 0.74.0
 */
final class Copies implements Iterable<Path> {

    /**
     * The directory of the build.
     */
    private final Path target;

    /**
     * Ctor.
     *
     * @param dir The directory of the build
     */
    Copies(final Path dir) {
        this.target = dir;
    }

    @Override
    public Iterator<Path> iterator() {
        return new Sorted<>(
            new Filtered<>(
                Files::isRegularFile,
                new Directory(this.target.resolve("7-lowering-planting"))
            )
        ).iterator();
    }
}
