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
import java.util.regex.Pattern;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;

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
        try {
            return new IoCheckedText(
                new Trimmed(
                    new TextOf(
                        new ResourceOf("org/eolang/lowering/phino-version.txt", this.getClass())
                    )
                )
            ).asString();
        } catch (final IOException ex) {
            throw new IllegalStateException(
                "Failed to read phino-version.txt from classpath", ex
            );
        }
    }

    /**
     * Dataize the merge of the given φ-calculus expressions.
     *
     * <p>Each expression must be complete on its own; {@code phino merge}
     * joins their root formations into one document, which is then
     * dataized. This is how a fragment meets the universe that holds the
     * method tables its references resolve against. The run also writes a
     * protocol of atom evaluations, and the term the last atom returned
     * names the carrier of the whole value, since the outermost atom
     * fires last; a run that fired no atom yields a value of unknown
     * forma, the way {@link Datum} explains.</p>
     *
     * @param expressions The expressions, in phi syntax
     * @return The value: its bytes and the term of the last evaluation
     * @throws IOException If the executable cannot be run
     */
    public Datum dataize(final String... expressions) throws IOException {
        final Path place = this.workspace();
        final Collection<Path> files = new ArrayList<>(expressions.length);
        try {
            final Path merged = this.merged(place, files, expressions);
            final Path protocol = Files.createTempFile(place, "evaluations", ".tsv");
            files.add(protocol);
            final String output = this.executed(
                this.binary, "dataize",
                "--max-steps", Integer.toString(this.steps),
                "--evaluations", protocol.toString(),
                merged.toString()
            );
            if (!Phino.HEX.matcher(output).matches()) {
                throw new IllegalStateException(
                    String.format(
                        "The dataization printed '%s', which is not data",
                        output
                    )
                );
            }
            return new Datum(output, Phino.answer(protocol));
        } finally {
            for (final Path file : files) {
                Files.deleteIfExists(file);
            }
        }
    }

    /**
     * Partially dataize the merge of the given φ-calculus expressions.
     *
     * <p>Under {@code --partial} an atom that cannot fire — a marker, or
     * a known atom whose input reaches one — parks in place instead of
     * failing the run, and lands in the protocol as a record with no
     * result term. The run then ends successfully either way: with data
     * when everything fired, with the residual expression when something
     * parked, and the records tell which sites did what. A genuinely
     * wrong expression, such as one reaching an error terminator, still
     * fails.</p>
     *
     * @param expressions The expressions, in phi syntax
     * @return The trace of the run: whether it was total, and its records
     * @throws IOException If the executable cannot be run
     */
    public Trace partial(final String... expressions) throws IOException {
        final Path place = this.workspace();
        final Collection<Path> files = new ArrayList<>(expressions.length);
        try {
            final Path merged = this.merged(place, files, expressions);
            final Path protocol = Files.createTempFile(place, "evaluations", ".tsv");
            files.add(protocol);
            final String output = this.executed(
                this.binary, "dataize",
                "--partial",
                "--max-steps", Integer.toString(this.steps),
                "--evaluations", protocol.toString(),
                merged.toString()
            );
            final List<Evaluation> records = new ArrayList<>(0);
            for (final String line : Files.readAllLines(protocol, StandardCharsets.UTF_8)) {
                if (!line.isEmpty()) {
                    records.add(new Evaluation(line));
                }
            }
            return new Trace(Phino.HEX.matcher(output).matches(), records);
        } finally {
            for (final Path file : files) {
                Files.deleteIfExists(file);
            }
        }
    }

    private Path merged(final Path place, final Collection<Path> files,
        final String... expressions) throws IOException {
        final Collection<String> command = new ArrayList<>(expressions.length + 4);
        command.add(this.binary);
        command.add("merge");
        for (final String expression : expressions) {
            final Path file = Files.createTempFile(place, "expression", ".phi");
            Files.write(file, expression.getBytes(StandardCharsets.UTF_8));
            files.add(file);
            command.add(file.toString());
        }
        final Path merged = Files.createTempFile(place, "merged", ".phi");
        files.add(merged);
        command.add("-t");
        command.add(merged.toString());
        this.executed(command.toArray(new String[0]));
        return merged;
    }

    private static String answer(final Path protocol) throws IOException {
        final List<String> lines = Files.readAllLines(protocol, StandardCharsets.UTF_8);
        final String term;
        if (lines.isEmpty()) {
            term = "";
        } else {
            final String last = lines.get(lines.size() - 1);
            term = last.substring(last.lastIndexOf('\t') + 1);
        }
        return term;
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
