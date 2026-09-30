/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import org.cactoos.Proc;
import org.cactoos.io.Directory;
import org.cactoos.iterable.Sorted;

/**
 * The deleting of a directory a stage fills anew on every build.
 *
 * <p>A stage that writes one file per entry it could fold has to start
 * from nothing, since an entry that folded in an earlier build and is a
 * taint now would otherwise leave its file behind, and javac or the
 * transpiler would go on reading it. The directory goes with all it holds,
 * the deepest paths first, and a directory that is not there is left as it
 * is.</p>
 *
 * @since 0.74.0
 */
final class Wiping implements Proc<Path> {

    @Override
    public void exec(final Path dir) throws IOException {
        if (Files.exists(dir)) {
            for (final Path stale
                : new Sorted<>(Comparator.reverseOrder(), new Directory(dir))) {
                Files.delete(stale);
            }
        }
    }
}
