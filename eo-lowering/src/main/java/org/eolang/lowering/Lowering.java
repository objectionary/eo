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
import org.cactoos.list.ListOf;

/**
 * The whole lowering, from the sources of a build to the Java it folds
 * them into.
 *
 * <p>Lowering happens over the whole world at once, and not a file at a
 * time, because a formation of one file is copied by objects of another
 * and the calculus has to see all of them together. So the stages here
 * are not a chain of independent tools: the first of them numbers every
 * formation of the build, and every stage after it speaks of a formation
 * by that number alone.</p>
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
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * The phino binary on this machine.
     */
    private final Phino phino;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory where the lowering keeps what it makes
     * @param exe The name or path of the phino executable
     */
    public Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final String exe
    ) {
        this(srcs, tbls, dir, new Phino(exe));
    }

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory where the lowering keeps what it makes
     * @param exe The phino binary on this machine
     */
    Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final Phino exe
    ) {
        this.sources = srcs;
        this.tables = tbls;
        this.home = dir;
        this.phino = exe;
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
        Files.createDirectories(this.home);
        for (final Stage stage : new ListOf<Stage>(
            new Planting(this.sources, this.tables, this.home),
            new Merging(this.home),
            new Running(this.home),
            new Patching(this.home),
            new Rendering(this.home)
        )) {
            stage.exec();
        }
    }
}
