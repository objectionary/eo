/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import com.yegor256.Jaxec;
import com.yegor256.Result;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;
import org.cactoos.text.UncheckedText;

/**
 * The phino program installed on this computer.
 *
 * <p>phino is an external program that knows the math of EO, which is
 * called the "phi-calculus". This module knows nothing about that math by
 * itself, and this class is the only place that runs phino. Every new
 * release of phino may read its input a little differently and may give
 * different results. So, this module trusts phino only when its version is
 * exactly the one written in the resource file
 * {@code phino-version.txt}.</p>
 *
 * <p>phino is started as a separate process through {@link Jaxec}. What it
 * prints is not shown in the log of the build. Its normal output is thrown
 * away, and its error output is shown only when phino fails. A reader of
 * the log should not be worried by lines that do not matter.</p>
 *
 * @since 0.74.0
 */
final class Phino {

    /**
     * The name of the phino program, or the path to it.
     */
    private final String binary;

    /**
     * Ctor.
     *
     * @param exe The name of the phino program, or the path to it
     */
    Phino(final String exe) {
        this.binary = exe;
    }

    @Override
    public String toString() {
        return this.binary;
    }

    /**
     * The version of the phino program on this computer.
     *
     * @return What {@code phino --version} prints, without spaces around it
     * @throws IOException If phino cannot be started
     */
    String version() throws IOException {
        final Path out = Files.createTempFile("phino", ".txt");
        try {
            final Result result = this.result(
                new Jaxec(this.binary, "--version")
                    .withStdout(ProcessBuilder.Redirect.to(out.toFile()))
                    .withStderr(ProcessBuilder.Redirect.DISCARD)
            );
            if (result.code() != 0) {
                throw new IOException(
                    String.format(
                        "The binary '%s' exited with code %d",
                        this.binary,
                        result.code()
                    )
                );
            }
            return new UncheckedText(new Trimmed(new TextOf(out))).asString();
        } finally {
            Files.deleteIfExists(out);
        }
    }

    /**
     * Join many XMIR files into one phi-expression, which is the world.
     *
     * <p>The world is written in the short form of the phi-calculus, which
     * is called "sweet". phino reads this file again once for every entry,
     * and there are thousands of entries, so every character saved in this
     * file saves a lot of work.</p>
     *
     * @param xmirs The XMIR files, in the order their objects must appear
     * @param world The file to write the world into
     * @throws IOException If phino cannot be started
     */
    void merge(final Iterable<Path> xmirs, final Path world) throws IOException {
        this.run(
            new Jaxec(
                this.binary,
                "merge",
                "--input=xmir",
                "--sweet",
                "--target",
                world.toString()
            ).with(new Mapped<>(Path::toString, xmirs)),
            String.format("merging the world into '%s'", world)
        );
    }

    /**
     * Ask phino to compute one entry of the world, with symbols as inputs.
     *
     * <p>This is called "morphing". The stage {@link Planting} put every
     * entry into an object named {@code l🌵}, under the name {@code e} plus
     * the number of the entry, and this method asks phino to work on
     * exactly that one. When phino meets an atom whose work is listed in
     * the table of operations, such as adding two numbers, it does not run
     * the atom, but writes down "here the two symbols were added". When
     * phino cannot go further, it leaves that part as it is. When phino
     * sees that it is going around in a circle, it stops that circle. phino
     * writes down every step it takes into the protocol file, and it writes
     * nothing else. If phino is still working when the time limit is over,
     * it is stopped, because one entry that never ends must not stop the
     * whole build.</p>
     *
     * @param world The world, which {@link Merging} wrote
     * @param atoms The table of operations phino may write down
     * @param entry The number of the entry to work on
     * @param protocol The file for the steps, in XML because its name ends with .xml
     * @param steps The largest number of steps phino may take inside one another
     * @param budget The time phino may work before it is stopped
     * @throws IOException If phino cannot be started, or a
     *  {@link KilledException} if phino was stopped because of the time limit
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    void morph(
        final Path world, final Path atoms, final int entry, final Path protocol,
        final int steps, final Duration budget
    ) throws IOException {
        final String task = String.format("morphing the entry %d of '%s'", entry, world);
        try {
            this.run(
                new Jaxec(
                    this.binary,
                    "morph",
                    "--deep",
                    "--acyclic=plausible",
                    "--partial",
                    "--quiet",
                    "--sweet",
                    "--hide-rho",
                    "--abridged",
                    String.format("--symbolic=%s", atoms),
                    String.format("--locator=Q.l🌵.e%d", entry),
                    String.format("--protocol=%s", protocol),
                    String.format("--max-steps=%d", steps),
                    world.toString()
                ).withTimeout(budget),
                task
            );
        } catch (final IllegalArgumentException ex) {
            throw new KilledException(
                Logger.format(
                    "The binary '%s' was killed after %[ms]s of %s",
                    this.binary, budget.toMillis(), task
                ),
                ex
            );
        }
    }

    /**
     * The only version of phino that this module accepts.
     *
     * @return The content of {@code phino-version.txt}, without spaces around it
     */
    String pin() {
        return new UncheckedText(
            new Trimmed(
                new TextOf(
                    new ResourceOf("org/eolang/lowering/phino-version.txt", this.getClass())
                )
            )
        ).asString();
    }

    private void run(final Jaxec command, final String task) throws IOException {
        final Path err = Files.createTempFile("phino", ".err");
        try {
            final Result result = this.result(
                command
                    .withStdout(ProcessBuilder.Redirect.DISCARD)
                    .withStderr(ProcessBuilder.Redirect.to(err.toFile()))
            );
            if (result.code() != 0) {
                throw new IllegalStateException(
                    String.format(
                        "The binary '%s' exited with code %d instead of %s: %s",
                        this.binary,
                        result.code(),
                        task,
                        new UncheckedText(new Trimmed(new TextOf(err))).asString()
                    )
                );
            }
        } finally {
            Files.deleteIfExists(err);
        }
    }

    private Result result(final Jaxec command) throws IOException {
        try {
            return command.withCheck(false).execUnsafe();
        } catch (final IOException ex) {
            throw new IOException(
                String.format("The binary '%s' cannot be started", this.binary),
                ex
            );
        }
    }
}
