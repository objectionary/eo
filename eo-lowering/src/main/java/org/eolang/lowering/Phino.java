/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;
import org.cactoos.text.UncheckedText;

/**
 * The phino binary on this machine.
 *
 * <p>Everything this module knows about φ-calculus lives in the external
 * {@code phino} executable, and this is the only class that talks to it.
 * The binary is trusted only when its version equals the one pinned in
 * the {@code phino-version.txt} resource, since the dialect it reads and
 * the rewriting it does change between releases. A run is bounded by an
 * explicit step budget and by seconds of wall clock: a binary that spins
 * or eats the machine is killed rather than allowed to take the build
 * down with it, and the run that was killed fails the same way a run the
 * binary itself refuses does.</p>
 *
 * <p>The subprocess runs with both of its streams redirected to files:
 * hundreds of fragments are tried per build and some runs are expected to
 * fail, so nothing the binary prints may reach the build log, where a line
 * saying {@code ERROR} would alarm for no reason. The scratch files live
 * in a directory the caller names, such as the target directory of the
 * build, never in the world-shared temporary one.</p>
 *
 * @since 0.76.0
 */
public final class Phino {

    /**
     * The name or path of the executable.
     */
    private final String binary;

    /**
     * The most rewriting steps one run may take.
     */
    private final int steps;

    /**
     * Where the scratch files go.
     */
    private final Path work;

    /**
     * How many seconds one run may take, where zero means no limit.
     */
    private final long seconds;

    /**
     * Ctor.
     *
     * @param exe The name or path of the executable
     * @param budget The most rewriting steps one run may take
     * @param dir Where the scratch files go
     */
    public Phino(final String exe, final int budget, final Path dir) {
        this(exe, budget, dir, 0L);
    }

    /**
     * Ctor.
     *
     * @param exe The name or path of the executable
     * @param budget The most rewriting steps one run may take
     * @param dir Where the scratch files go
     * @param span How many seconds one run may take, where zero means no limit
     */
    public Phino(final String exe, final int budget, final Path dir, final long span) {
        this.binary = exe;
        this.steps = budget;
        this.work = dir;
        this.seconds = span;
    }

    /**
     * The version the binary reports.
     *
     * @return The version, such as {@code 0.0.127}
     * @throws IOException If the binary cannot be run
     */
    public String version() throws IOException {
        return this.executed(this.binary, "--version");
    }

    /**
     * Whether the binary is there and of the pinned version.
     *
     * @return True if runs may be trusted to it
     */
    public boolean suitable() {
        boolean good;
        try {
            good = this.version().equals(this.pin());
        } catch (final IOException | IllegalStateException ex) {
            good = false;
        }
        return good;
    }

    /**
     * The version this module was written against.
     *
     * @return The version, such as {@code 0.0.127}
     */
    public String pin() {
        return new UncheckedText(
            new Trimmed(
                new TextOf(
                    new ResourceOf("org/eolang/lowering/phino-version.txt", this.getClass())
                )
            )
        ).asString();
    }

    /**
     * Merge XMIR documents into one φ-expression, the world of a run.
     *
     * @param docs The XMIR files
     * @param target Where the world goes
     * @throws IOException If the binary fails
     */
    public void merged(final List<Path> docs, final Path target) throws IOException {
        final Collection<String> command = new ArrayList<>(docs.size() + 5);
        command.add(this.binary);
        command.add("merge");
        command.add("--input=xmir");
        for (final Path doc : docs) {
            command.add(doc.toString());
        }
        command.add("-t");
        command.add(target.toString());
        this.executed(command.toArray(new String[0]));
    }

    /**
     * Morph one expression inside a universe, deep and partial, through a registry of atoms.
     *
     * @param world The universe, a φ-expression
     * @param inside The expression, a dispatch from Φ into the fragment
     * @param registry The {@code atoms.json} file
     * @return The residual of the expression, a φ-expression
     * @throws IOException If the binary fails
     */
    public String morphed(final Path world, final String inside, final Path registry)
        throws IOException {
        return this.executed(
            this.binary, "morph",
            "--deep", "--partial", "--hide-rho",
            String.format("--atoms=%s", registry),
            String.format("--max-steps=%d", this.steps),
            String.format("--inside=%s", inside),
            world.toString()
        );
    }

    /**
     * Print a φ-expression as XMIR, without comments and without the listing.
     *
     * @param phi The file with the expression, a single binding at the top
     * @return The XMIR
     * @throws IOException If the binary cannot be run or exits with an error
     */
    public String xmir(final Path phi) throws IOException {
        return this.executed(
            this.binary, "rewrite",
            "--output=xmir", "--omit-listing", "--omit-comments",
            phi.toString()
        );
    }

    private String executed(final String... command) throws IOException {
        final Path place = Files.createDirectories(this.work);
        final Path out = Files.createTempFile(place, "phino", ".out");
        final Path err = Files.createTempFile(place, "phino", ".err");
        try {
            final int code = this.waited(
                new ProcessBuilder(command)
                    .redirectOutput(out.toFile())
                    .redirectError(err.toFile())
                    .start(),
                command
            );
            if (code != 0) {
                throw new IllegalStateException(
                    String.format(
                        "The binary '%s' exited with code %d: %s",
                        this.binary,
                        code,
                        Files.readString(err, StandardCharsets.UTF_8).trim()
                    )
                );
            }
            return Files.readString(out, StandardCharsets.UTF_8).trim();
        } finally {
            Files.deleteIfExists(out);
            Files.deleteIfExists(err);
        }
    }

    private int waited(final Process proc, final String... command) throws IOException {
        try {
            final boolean done;
            if (this.seconds > 0L) {
                done = proc.waitFor(this.seconds, TimeUnit.SECONDS);
            } else {
                proc.waitFor();
                done = true;
            }
            if (!done) {
                proc.destroyForcibly().waitFor();
                throw new IllegalStateException(
                    String.format(
                        "The binary '%s' took longer than %d second(s) and was killed: %s",
                        this.binary, this.seconds, String.join(" ", command)
                    )
                );
            }
            return proc.exitValue();
        } catch (final InterruptedException ex) {
            proc.destroyForcibly();
            Thread.currentThread().interrupt();
            throw new IOException(
                String.format("Interrupted while waiting for '%s'", this.binary), ex
            );
        }
    }
}
