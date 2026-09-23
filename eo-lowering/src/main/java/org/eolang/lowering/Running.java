/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.io.ResourceOf;

/**
 * The one running of phino over the world.
 *
 * <p>Every formation of the build is folded by a single call of the
 * binary. The call is aimed at the entries, it enters each of them in the
 * order they were written, and the protocol it writes beside the world is
 * the only thing the stages after this one read. One run rather than one
 * per formation, because the world is parsed once and a formation that
 * calls another is folded with the arithmetic of that other one already
 * unfolded into it.</p>
 *
 * <p>What the binary can say about a primitive is said in
 * {@code atoms.yaml} and nowhere else, and that table is written beside
 * the world from the resource of the same name before the run, so that
 * what phino was told stays next to what it answered. A lambda no entry
 * of that file matches is left standing where it is, and the formation
 * that reached it is a taint: nothing is guessed about it, and nothing is
 * folded.</p>
 *
 * <p>The run is bounded by a ceiling of nested steps, because a formation
 * that grows on every round, a loop counting up for one, never comes back
 * to a term it has seen, so no guard against cycles can stop it. At the
 * ceiling the binary leaves that formation standing as a taint and goes
 * on to the next entry, and the build fails only when the binary itself
 * exits with an error. Nothing is retried and nothing is skipped.</p>
 *
 * @since 0.74.0
 */
final class Running implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * The binary that runs.
     */
    private final Phino phino;

    /**
     * The ceiling of nested morphing and dataization steps.
     */
    private final int steps;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     * @param exe The binary that runs
     */
    Running(final Path dir, final Phino exe) {
        this(dir, exe, 32);
    }

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     * @param exe The binary that runs
     * @param ceiling The ceiling of nested morphing and dataization steps
     */
    Running(final Path dir, final Phino exe, final int ceiling) {
        this.home = dir;
        this.phino = exe;
        this.steps = ceiling;
    }

    @Override
    public void exec() throws IOException {
        final Path world = this.home.resolve("world.phi");
        if (!Files.exists(world)) {
            throw new IllegalStateException(
                String.format(
                    "There is no '%s', while running needs the world the merging writes",
                    world
                )
            );
        }
        final Path atoms = Files.write(
            this.home.resolve("atoms.yaml"),
            new UncheckedBytes(
                new BytesOf(new ResourceOf("org/eolang/lowering/atoms.yaml"))
            ).asBytes()
        );
        final Path protocol = this.home.resolve("protocol.xml");
        final long start = System.currentTimeMillis();
        this.phino.morph(world, atoms, protocol, this.steps);
        Logger.info(
            this,
            "Ran phino over %[file]s in %[ms]s, up to %d nested steps, into %[file]s (%[size]s)",
            world,
            System.currentTimeMillis() - start,
            this.steps,
            protocol,
            Files.size(protocol)
        );
    }
}
