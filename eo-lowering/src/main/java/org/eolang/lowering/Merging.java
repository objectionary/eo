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
 * The stage that puts all the objects of the build into one file.
 *
 * <p>phino does not know anything about files. But an object in one file
 * often uses an object from another file. So, before phino can work, all
 * the objects must be in one place. This stage asks phino to join all the
 * XMIR files of the build and the file of the entries into one big
 * phi-expression, which is saved in the file {@code world.phi}. This file
 * is called the "world". Later stages ask phino questions only about the
 * world, and never about the separate files.</p>
 *
 * <p>The XMIR files that come here are the copies that {@link Pruning}
 * wrote, so they have no tests inside. The world holds only the objects,
 * and nothing that tests them.</p>
 *
 * <p>This stage calls phino exactly once. It cannot be done in parts,
 * because every entry has a number, and that number means something only
 * inside this one world. The files always go in the same order, with the
 * entries last, so the world is exactly the same on every build of the
 * same program. If phino fails, the build fails too, and the error shows
 * what phino printed. There is nothing useful the next stages can do
 * without the world.</p>
 *
 * @since 0.74.0
 */
final class Merging implements Proc<Path> {

    /**
     * The phino program, which joins the files.
     */
    private final Phino phino;

    /**
     * Ctor.
     *
     * @param exe The phino program, which joins the files
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
