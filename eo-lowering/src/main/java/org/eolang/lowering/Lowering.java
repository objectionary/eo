/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import org.cactoos.Proc;
import org.cactoos.list.ListOf;
import org.cactoos.proc.ForEach;
import org.cactoos.proc.IoCheckedProc;
import org.eolang.cache.GlobalCache;

/**
 * The whole lowering, from the sources of a build to the Java it folds
 * them into.
 *
 * <p>Lowering happens over the whole world at once, and not a file at a
 * time, because a formation of one file is copied by objects of another
 * and the calculus has to see all of them together. So the stages here
 * are not a chain of independent tools: the first of them cuts the tests
 * out of every source, the second numbers every formation of what is
 * left, and every stage after that speaks of a formation by that number
 * alone. What the stages make lives under the directory of the build:
 * the sources with their tests cut out in {@code 7-lowering-planting}, the
 * world and everything on the way to it in {@code 7-lowering}, and the
 * protocol of every entry, one file per object morphed, beside it in
 * {@code 7-lowering-protocols}.</p>
 *
 * <p>Nothing on the way is optional. A stage that cannot read what the
 * one before it wrote, a binary of the wrong version, a run that reaches
 * its step limit — each of them fails the build, since a lowering that
 * quietly skipped a formation would leave a program whose Java nobody can
 * account for.</p>
 *
 * @since 0.74.0
 * @todo #8548:30min Run the stages through {@code Procs} of Cactoos. The
 *  stages are applied to the directory of the build by a {@code ForEach}
 *  over the list of them, which turns the list into the argument and hides
 *  the directory in a lambda. Once yegor256/cactoos#1960 is released, put
 *  them into one {@code Procs} and apply it to the directory instead.
 */
public final class Lowering {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory with the tables of {@code eo:inference}.
     */
    private final Path tables;

    /**
     * The directory of the build, {@code target/eo}, where the lowering
     * makes the directories it keeps what it makes in.
     */
    private final Path target;

    /**
     * The phino binary on this machine.
     */
    private final Phino phino;

    /**
     * The cache the protocols of the morphing are kept in between builds.
     */
    private final GlobalCache cache;

    /**
     * The directory of generated sources the atoms are written into.
     */
    private final Path generated;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory of the build, {@code target/eo}
     * @param exe The name or path of the phino executable
     * @param store The cache the protocols of the morphing are kept in
     * @param gen The directory of generated sources the atoms are written into
     */
    public Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final String exe,
        final GlobalCache store, final Path gen
    ) {
        this(srcs, tbls, dir, new Phino(exe), store, gen);
    }

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory of the build, {@code target/eo}
     * @param exe The phino binary on this machine
     * @param store The cache the protocols of the morphing are kept in
     * @param gen The directory of generated sources the atoms are written into
     */
    Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final Phino exe,
        final GlobalCache store, final Path gen
    ) {
        this.sources = srcs;
        this.tables = tbls;
        this.target = dir;
        this.phino = exe;
        this.cache = store;
        this.generated = gen;
    }

    /**
     * Fold the formations of the whole build.
     *
     * @throws IOException If anything the lowering needs cannot be read
     *  or written
     */
    public void exec() throws IOException {
        final String pinned = this.phino.pin();
        final String found;
        try {
            found = this.phino.version();
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "The binary '%s' cannot run, while lowering needs phino %s",
                    this.phino,
                    pinned
                ),
                ex
            );
        }
        if (!found.equals(pinned)) {
            throw new IllegalStateException(
                String.format(
                    "The binary '%s' is of version %s, while lowering needs phino %s",
                    this.phino,
                    found,
                    pinned
                )
            );
        }
        Logger.info(
            this,
            "Phino %s is found at '%s', though nothing is lowered yet",
            pinned,
            this.phino
        );
        new IoCheckedProc<>(
            new ForEach<Proc<Path>>(stage -> stage.exec(this.target))
        ).exec(
            new ListOf<>(
                new Pruning(this.sources),
                new Planting(this.tables),
                new Merging(this.phino),
                new Morphing(this.phino, this.cache),
                new Patching(),
                new Rendering(this.generated)
            )
        );
    }
}
