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
import java.util.concurrent.TimeoutException;
import org.cactoos.io.ResourceOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;
import org.cactoos.text.UncheckedText;

/**
 * The phino binary on this machine.
 *
 * <p>Everything the lowering knows about the calculus lives in the
 * external {@code phino} executable, and this is the only class that talks
 * to it. The binary is trusted only when the version it reports is the one
 * pinned in the {@code phino-version.txt} resource, since both the dialect
 * it reads and the rewriting it does change from release to release.</p>
 *
 * <p>The subprocess runs through {@link Jaxec} with both of its streams
 * sent away from the terminal, because what the binary prints belongs to
 * the caller and not to the build log, where a stray line of it would
 * alarm a reader for no reason.</p>
 *
 * @since 0.74.0
 */
final class Phino {

    /**
     * The name or path of the executable.
     */
    private final String binary;

    /**
     * Ctor.
     *
     * @param exe The name or path of the executable
     */
    Phino(final String exe) {
        this.binary = exe;
    }

    @Override
    public String toString() {
        return this.binary;
    }

    /**
     * The version the executable reports.
     *
     * @return The trimmed output of {@code phino --version}
     * @throws IOException If the executable cannot be run
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
     * Merge XMIR files into one phi-expression.
     *
     * <p>The world is printed with syntax sugar, and the protocol of the
     * run also without a single ρ binding, since both are read back by
     * this binary or by the stages after the run, and every glyph saved
     * there is saved on every one of the thousands of entries.</p>
     *
     * @param xmirs The XMIR files, in the order their objects are to stand
     * @param world The file to write the merged expression to
     * @throws IOException If the executable cannot be run
     */
    void merge(final Iterable<Path> xmirs, final Path world) throws IOException {
        this.run(
            new Jaxec(
                this.binary, "merge", "--input=xmir", "--sweet", "--target", world.toString()
            ).with(new Mapped<>(Path::toString, xmirs)),
            String.format("merging the world into '%s'", world)
        );
    }

    /**
     * Morph one entry of a world symbolically.
     *
     * <p>The run is aimed at one mark of the {@code l🌵} object the
     * planting writes, the one numbered as the entry is, answers the λ
     * functions the table names, leaves standing what it cannot answer,
     * stops a term that comes back to itself, and writes nothing but the
     * protocol. A run that is still going when its budget is spent is
     * killed, since one entry that never ends must not hold the build.</p>
     *
     * @param world The merged world
     * @param atoms The table of operations the run may answer
     * @param entry The number of the entry to morph
     * @param protocol The file to record every firing into, XML by its name
     * @param steps The ceiling of nested morphing and dataization steps
     * @param budget The time the run may take before it is killed
     * @throws IOException If the executable cannot be run
     * @throws TimeoutException If the run was killed over its budget
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    void morph(
        final Path world, final Path atoms, final int entry, final Path protocol,
        final int steps, final Duration budget
    ) throws IOException, TimeoutException {
        final String task = String.format("morphing the entry %d of '%s'", entry, world);
        try {
            this.run(
                new Jaxec(
                    this.binary, "morph",
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
            final TimeoutException killed = new TimeoutException(
                Logger.format(
                    "The binary '%s' was killed after %[ms]s of %s",
                    this.binary, budget.toMillis(), task
                )
            );
            killed.initCause(ex);
            throw killed;
        }
    }

    /**
     * The version the lowering is pinned to.
     *
     * @return The trimmed content of the {@code phino-version.txt} resource
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
