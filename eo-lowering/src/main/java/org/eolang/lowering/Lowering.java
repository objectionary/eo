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
 * All the work of this module, from the sources of a build to Java atoms.
 *
 * <p>This is the only public class of the module. It checks that the
 * right version of phino is installed, and then runs all the stages, one
 * after another. The work is done on all the objects of the build at once,
 * and not on one file at a time. The reason is that an object in one file
 * often uses objects from other files, and phino has to see all of them
 * together.</p>
 *
 * <p>Because of this, the stages are not separate tools. They depend on
 * each other. The first stage removes the tests from every source. The
 * second stage gives a number to every object that may become an atom,
 * and all the next stages talk about an object only by that number.</p>
 *
 * <p>The stages keep what they make in one home directory, which is given
 * to the constructor, usually {@code target/eo/NN-lowering}:</p>
 *
 * <ul>
 * <li>the home directory itself holds the world and the other files that
 * are made on the way to it;</li>
 * <li>{@code 1-planting}, inside the home directory, holds the copies of the
 * sources, without the tests;</li>
 * <li>{@code 2-protocols}, inside the home directory, holds one protocol file
 * for every entry that phino worked on;</li>
 * <li>{@code 2-protocols-txt}, inside the home directory, holds the same
 * protocols as indented text, only when they are asked for;</li>
 * <li>the directory of atoms, given to the constructor, holds the Java
 * atoms;</li>
 * <li>the directory of patched sources, given to the constructor, holds
 * the XMIR files where atoms took the place of the bodies of
 * objects.</li>
 * </ul>
 *
 * <p>The number at the start of the name of a directory tells in which
 * order the stages make these directories.</p>
 *
 * <p>No step can be skipped. The build fails when a stage cannot read what
 * the stage before it wrote, and when phino has the wrong version. If an
 * object were quietly skipped, nobody could explain the Java of the
 * program later.</p>
 *
 * @since 0.64.0
 */
public final class Lowering {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory with the tables of {@code eo:inference}, which say the
     * types of the voids.
     */
    private final Path tables;

    /**
     * The home directory of the lowering, usually
     * {@code target/eo/NN-lowering}, where the stages keep what they make.
     */
    private final Path home;

    /**
     * The phino program on this computer.
     */
    private final Phino phino;

    /**
     * The cache, where the protocols are kept from one build to the next.
     */
    private final GlobalCache cache;

    /**
     * The directory for the Java atoms, which javac compiles.
     */
    private final Path atoms;

    /**
     * The directory for the XMIR files where atoms took the place of the
     * bodies of objects, which the transpiler reads.
     */
    private final Path patched;

    /**
     * The entries that phino is allowed to run on.
     */
    private final Scope scope;

    /**
     * The largest number of steps inside one another that one run of phino
     * on one entry may take.
     */
    private final int steps;

    /**
     * The time that one run of phino on one entry may take before it is
     * stopped.
     */
    private final Duration budget;

    /**
     * Whether every entry gets a protocol in text too.
     */
    private final boolean texts;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The home directory of the lowering, usually {@code target/eo/NN-lowering}
     * @param exe The name of the phino program, or the path to it
     * @param store The cache, where the protocols are kept between builds
     * @param kept The directory for the Java atoms, which javac compiles
     * @param copies The directory for the patched XMIR files
     * @param range The entries that phino is allowed to run on
     * @param ceiling The largest number of steps inside one another that one
     *  run of phino may take
     * @param span The time that one run of phino may take before it is stopped
     * @param text Whether every entry gets a protocol in text too
     */
    public Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final String exe,
        final GlobalCache store, final Path kept, final Path copies, final Scope range,
        final int ceiling, final Duration span, final boolean text
    ) {
        this(srcs, tbls, dir, new Phino(exe), store, kept, copies, range, ceiling, span, text);
    }

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The home directory of the lowering, usually {@code target/eo/NN-lowering}
     * @param exe The phino program on this computer
     * @param store The cache, where the protocols are kept between builds
     * @param kept The directory for the Java atoms, which javac compiles
     * @param copies The directory for the patched XMIR files
     * @param range The entries that phino is allowed to run on
     * @param ceiling The largest number of steps inside one another that one
     *  run of phino may take
     * @param span The time that one run of phino may take before it is stopped
     * @param text Whether every entry gets a protocol in text too
     */
    Lowering(
        final Collection<Path> srcs, final Path tbls, final Path dir, final Phino exe,
        final GlobalCache store, final Path kept, final Path copies, final Scope range,
        final int ceiling, final Duration span, final boolean text
    ) {
        this.sources = srcs;
        this.tables = tbls;
        this.home = dir;
        this.phino = exe;
        this.cache = store;
        this.atoms = kept;
        this.patched = copies;
        this.scope = range;
        this.steps = ceiling;
        this.budget = span;
        this.texts = text;
    }

    /**
     * Whether the phino program can be started on this computer at all.
     *
     * <p>This method only checks that phino starts and prints its version.
     * It does not check that the version is the right one. A wrong version
     * still makes {@link #exec()} fail.</p>
     *
     * @return TRUE if phino can be started, FALSE if it cannot
     */
    public boolean available() {
        boolean found;
        try {
            this.phino.version();
            found = true;
        } catch (final IOException ex) {
            Logger.debug(
                this, "The binary '%s' cannot be started: %s", this.phino, ex.getMessage()
            );
            found = false;
        }
        return found;
    }

    /**
     * Run all the stages on the whole build.
     *
     * @throws IOException If a file that a stage needs cannot be read or
     *  written
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
                new Morphing(
                    this.phino, this.cache, this.scope, this.steps, this.budget, this.texts
                ),
                new Rendering(this.atoms, this.tables),
                new Patching(this.sources, this.tables, this.patched)
            )
        ).exec(this.home);
    }
}
