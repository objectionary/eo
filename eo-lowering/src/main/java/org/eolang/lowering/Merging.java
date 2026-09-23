/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import org.cactoos.iterable.Joined;
import org.cactoos.iterable.Sorted;

/**
 * The merging of every object of the build into one phi-expression.
 *
 * <p>The calculus knows nothing of files. A formation that copies an
 * object of another file has to find that object where it stands, so the
 * XMIR files of the build and the entries are joined into a single
 * document, and it is that document, and never a file of it, that the
 * evaluation is asked about. The tests of an object stay in it, since an
 * object and what is said about it are one document in this compiler.</p>
 *
 * <p>There is one call and no second one, because the number an entry
 * carries means nothing outside the one world it was written for. The
 * sources go in a fixed order and the entries last, so the world comes
 * out the same on every run. A call that fails fails the build with what
 * the binary printed, since a world that was not merged cannot be
 * evaluated and there is nothing sensible for a later stage to do about
 * it.</p>
 *
 * @since 0.74.0
 */
final class Merging implements Stage {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * The binary that merges.
     */
    private final Phino phino;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param dir The directory where the lowering keeps what it makes
     * @param exe The binary that merges
     */
    Merging(final Collection<Path> srcs, final Path dir, final Phino exe) {
        this.sources = srcs;
        this.home = dir;
        this.phino = exe;
    }

    @Override
    public void exec() throws IOException {
        final Path entries = this.home.resolve("entries.xmir");
        if (!Files.exists(entries)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while merging needs the entries the planting writes",
                    entries
                )
            );
        }
        final Path world = this.home.resolve("world.phi");
        this.phino.merge(
            new Joined<Path>(new Sorted<>(this.sources), Collections.singletonList(entries)),
            world
        );
        Logger.info(
            this,
            "Merged %d XMIR files and the entries into %[file]s (%[size]s)",
            this.sources.size(),
            world,
            Files.size(world)
        );
    }
}
