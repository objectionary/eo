/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Collection;
import org.cactoos.proc.IoCheckedProc;
import org.cactoos.proc.Procs;
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
     * The directory the atoms are written into, which javac compiles.
     */
    private final Path atoms;

    /**
     * The time one run of phino may take on one entry before it is killed.
     */
    private final Duration budget;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory of the build, {@code target/eo}
     * @param exe The name or path of the phino executable
     * @param store The cache the protocols of the morphing are kept in
     * @param kept The directory the atoms are written into, which javac compiles
     * @param span The time one run of phino may take before it is killed
     */
    public Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final String exe,
        final GlobalCache store, final Path kept, final Duration span
    ) {
        this(srcs, tbls, dir, new Phino(exe), store, kept, span);
    }

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory of the build, {@code target/eo}
     * @param exe The phino binary on this machine
     * @param store The cache the protocols of the morphing are kept in
     * @param kept The directory the atoms are written into, which javac compiles
     * @param span The time one run of phino may take before it is killed
     */
    Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final Phino exe,
        final GlobalCache store, final Path kept, final Duration span
    ) {
        this.sources = srcs;
        this.tables = tbls;
        this.target = dir;
        this.phino = exe;
        this.cache = store;
        this.atoms = kept;
        this.budget = span;
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
            new Procs<>(
                new Pruning(this.sources),
                new Planting(this.tables),
                new Merging(this.phino),
                new Morphing(this.phino, this.cache, this.budget),
                new Patching(),
                new Rendering(this.atoms)
            )
        ).exec(this.target);
    }
}
