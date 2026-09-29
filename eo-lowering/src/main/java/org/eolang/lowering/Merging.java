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
import org.cactoos.Proc;
import org.cactoos.iterable.Joined;
import org.cactoos.list.ListOf;

/**
 * The merging of every object of the build into one phi-expression.
 *
 * <p>The calculus knows nothing of files. A formation that copies an
 * object of another file has to find that object where it stands, so the
 * XMIR files of the build and the entries are joined into a single
 * document, and it is that document, and never a file of it, that the
 * evaluation is asked about. The sources that arrive here are the copies
 * the pruning wrote, with the tests cut out, so the world holds the
 * objects and nothing that is said about them.</p>
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
final class Merging implements Proc<Path> {

    /**
     * The binary that merges.
     */
    private final Phino phino;

    /**
     * Ctor.
     *
     * @param exe The binary that merges
     */
    Merging(final Phino exe) {
        this.phino = exe;
    }

    @Override
    public void exec(final Path target) throws IOException {
        final Path home = target.resolve("7-lowering");
        final Path entries = home.resolve("entries.xmir");
        if (!Files.exists(entries)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while merging needs the entries the planting writes",
                    entries
                )
            );
        }
        final Collection<Path> sources = new ListOf<>(new Copies(target));
        final Path world = home.resolve("world.phi");
        this.phino.merge(
            new Joined<Path>(sources, Collections.singletonList(entries)),
            world
        );
        Logger.info(
            this,
            "Merged %d XMIR files and the entries into %[file]s (%[size]s)",
            sources.size(),
            world,
            Files.size(world)
        );
    }
}
