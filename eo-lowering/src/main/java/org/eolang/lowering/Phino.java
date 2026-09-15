/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Jaxec;
import com.yegor256.Result;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
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
public final class Phino {

    /**
     * The name or path of the executable.
     */
    private final String binary;

    /**
     * Ctor.
     *
     * @param exe The name or path of the executable
     */
    public Phino(final String exe) {
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
    public String version() throws IOException {
        final Path out = Files.createTempFile("phino", ".txt");
        try {
            this.run(out);
            return new UncheckedText(new Trimmed(new TextOf(out))).asString();
        } finally {
            Files.deleteIfExists(out);
        }
    }

    /**
     * The version the lowering is pinned to.
     *
     * @return The trimmed content of the {@code phino-version.txt} resource
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

    private void run(final Path out) throws IOException {
        final Result result;
        try {
            result = new Jaxec(this.binary, "--version")
                .withCheck(false)
                .withStdout(ProcessBuilder.Redirect.to(out.toFile()))
                .withStderr(ProcessBuilder.Redirect.DISCARD)
                .execUnsafe();
        } catch (final IOException ex) {
            throw new IOException(
                String.format("The binary '%s' cannot be started", this.binary),
                ex
            );
        }
        if (result.code() != 0) {
            throw new IOException(
                String.format(
                    "The binary '%s' exited with code %d",
                    this.binary,
                    result.code()
                )
            );
        }
    }
}
