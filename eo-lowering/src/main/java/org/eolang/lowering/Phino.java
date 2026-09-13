/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Jaxec;
import com.yegor256.Result;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
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
 * explicit step budget.</p>
 *
 * <p>The subprocess runs through {@link Jaxec}, with both of its streams
 * redirected to files: hundreds of fragments are tried per build and some
 * runs are expected to fail, so nothing the binary prints may reach the
 * build log, where a line saying {@code ERROR} would alarm for no reason.
 * The scratch files live in a directory the caller names, such as the
 * target directory of the build, never in the world-shared temporary
 * one.</p>
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
     * Ctor.
     *
     * @param exe The name or path of the executable
     * @param budget The most rewriting steps one run may take
     * @param dir Where the scratch files go
     */
    public Phino(final String exe, final int budget, final Path dir) {
        this.binary = exe;
        this.steps = budget;
        this.work = dir;
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
     * Morph one object of a universe, deep and partial, through a registry of atoms.
     *
     * @param world The universe, a φ-expression
     * @param inside The locator of the object, such as {@code Φ.demo.gap}
     * @param registry The {@code atoms.json} file
     * @return The residual of the object, as XMIR
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
            "--output=xmir", "--omit-listing",
            world.toString()
        );
    }

    /**
     * Run the binary and read what it printed.
     *
     * @param command The command line
     * @return The standard output, trimmed
     * @throws IOException If the binary cannot be run or exits with an error
     */
    private String executed(final String... command) throws IOException {
        final Path place = Files.createDirectories(this.work);
        final Path out = Files.createTempFile(place, "phino", ".out");
        final Path err = Files.createTempFile(place, "phino", ".err");
        try {
            final Result result = new Jaxec(command)
                .withCheck(false)
                .withStdout(ProcessBuilder.Redirect.to(out.toFile()))
                .withStderr(ProcessBuilder.Redirect.to(err.toFile()))
                .execUnsafe();
            if (result.code() != 0) {
                throw new IllegalStateException(
                    String.format(
                        "The binary '%s' exited with code %d: %s",
                        this.binary,
                        result.code(),
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
}
