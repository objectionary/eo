/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Jaxec;
import com.yegor256.Result;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;

/**
 * The phino binary on this machine.
 *
 * <p>Everything this module knows about φ-calculus lives in the external
 * {@code phino} executable, and this is the only class that talks to it.
 * The binary is trusted only when its version equals the one pinned in
 * the {@code phino-version.txt} resource, since the dialect it reads and
 * the rewriting it does change between releases. A dataization is bounded
 * by an explicit step budget, and its output is accepted only when it
 * looks like data — phino has been seen reporting an error on stdout with
 * a zero exit code, so the exit code alone proves nothing.</p>
 *
 * <p>The subprocess runs through {@link Jaxec}, with both of its streams
 * redirected to files: hundreds of fragments are tried per build and most
 * refusals are expected, so nothing the binary prints may reach the build
 * log, where a line saying {@code ERROR} would alarm for no reason. The
 * scratch files live in a directory the caller names, such as the target
 * directory of the build, never in the world-shared temporary one.</p>
 *
 * @since 0.76.0
 */
public final class Phino {

    /**
     * What dataized bytes look like: empty, one byte, or dash-joined pairs.
     */
    private static final Pattern HEX = Pattern.compile(
        "--|[0-9A-F]{2}-|[0-9A-F]{2}(-[0-9A-F]{2})+"
    );

    /**
     * The name or path of the executable.
     */
    private final String binary;

    /**
     * The most rewriting steps one dataization may take.
     */
    private final int steps;

    /**
     * The directory for the scratch files of the subprocess.
     */
    private final Path work;

    /**
     * Ctor.
     * @param exe The name or path of the executable
     * @param budget The most rewriting steps one dataization may take
     * @param dir The directory for the scratch files of the subprocess
     */
    public Phino(final String exe, final int budget, final Path dir) {
        this.binary = exe;
        this.steps = budget;
        this.work = dir;
    }

    /**
     * The version the executable reports.
     * @return The trimmed output of {@code phino --version}
     * @throws IOException If the executable cannot be run
     */
    public String version() throws IOException {
        return this.executed(this.binary, "--version");
    }

    /**
     * Whether the executable is present and of the pinned version.
     * @return True if every answer of this binary can be trusted
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
     * The version this module is pinned to.
     * @return The trimmed content of the {@code phino-version.txt} resource
     */
    public String pin() {
        try (InputStream stream = this.getClass().getResourceAsStream("phino-version.txt")) {
            return new String(
                stream.readAllBytes(), StandardCharsets.UTF_8
            ).trim();
        } catch (final IOException ex) {
            throw new IllegalStateException(
                "Failed to read phino-version.txt from classpath", ex
            );
        }
    }

    /**
     * Dataize one φ-calculus document.
     * @param document The document, in phi syntax
     * @return The dataized bytes, as dash-joined hex pairs
     * @throws IOException If the executable cannot be run
     */
    public String dataize(final String document) throws IOException {
        final Path file = Files.createTempFile(this.workspace(), "universe", ".phi");
        try {
            Files.write(file, document.getBytes(StandardCharsets.UTF_8));
            final String output = this.executed(
                this.binary, "dataize",
                "--max-steps", Integer.toString(this.steps),
                file.toString()
            );
            if (!Phino.HEX.matcher(output).matches()) {
                throw new IllegalStateException(
                    String.format(
                        "The dataization printed '%s', which is not data",
                        output
                    )
                );
            }
            return output;
        } finally {
            Files.deleteIfExists(file);
        }
    }

    private String executed(final String... command) throws IOException {
        final Path place = this.workspace();
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

    private Path workspace() throws IOException {
        return Files.createDirectories(this.work);
    }
}
