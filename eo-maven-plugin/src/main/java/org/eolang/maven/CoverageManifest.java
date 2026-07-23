/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The full set of locations the transpiler wrapped into {@code PhCoverage},
 * one line per location: symbolic locator and source line, tab-separated.
 * Built once per {@code transpile} run (see {@link Transpiling}), then read
 * back by {@link MjCoverageReport}, which merges it against the raw hits
 * recorded at runtime by {@code PhCoverage} itself, to tell which
 * instrumented lines were actually touched. The manifest does not record a
 * source file for each location: {@link MjCoverageReport} resolves that by
 * matching a locator's leading object id against the already-existing
 * {@code eo-foreign} catalog, instead of duplicating that mapping here.
 * @since 0.58
 */
final class CoverageManifest {

    /**
     * Matches one {@code new PhCoverage(name, "loc", line, pos);} call, as
     * emitted by {@code to-java.xsl}, in the transpiled Java text.
     */
    private static final Pattern WRAPPED = Pattern.compile(
        "new PhCoverage\\([^,]+,\\s*\"([^\"]*)\",\\s*(\\d+),\\s*\\d+\\)"
    );

    /**
     * The raw coverage hits file this manifest is paired with.
     */
    private final Path hits;

    /**
     * Ctor.
     * @param touched The raw coverage hits file this manifest is paired with
     */
    CoverageManifest(final Path touched) {
        this.hits = touched;
    }

    /**
     * Where this manifest is stored: a sibling of the hits file.
     * @return The path
     */
    Path path() {
        return Paths.get(String.format("%s.manifest", this.hits));
    }

    /**
     * Start a fresh manifest, discarding whatever a previous run left behind.
     * @throws IOException If fails to delete
     */
    void reset() throws IOException {
        Files.deleteIfExists(this.path());
    }

    /**
     * Scan a transpiled source for {@code PhCoverage} wrappers and append
     * one manifest line per location found, in a single write. Just like
     * {@code PhCoverage} itself, this relies on the OS append guarantee for
     * a single {@code write()} syscall to stay safe across the transpiler's
     * worker threads, with no explicit locking.
     * @param java The transpiled Java (or XMIR-with-Java) text to scan
     * @throws IOException If fails to append
     */
    void scan(final String java) throws IOException {
        final Matcher matcher = CoverageManifest.WRAPPED.matcher(java);
        final List<String> lines = new ArrayList<>(0);
        while (matcher.find()) {
            lines.add(
                String.format(
                    "%s\t%s%n", matcher.group(1), matcher.group(2)
                )
            );
        }
        if (!lines.isEmpty()) {
            final StringBuilder joined = new StringBuilder(lines.size() * 32);
            for (final String line : lines) {
                joined.append(line);
            }
            Files.write(
                this.path(),
                joined.toString().getBytes(StandardCharsets.UTF_8),
                StandardOpenOption.CREATE, StandardOpenOption.APPEND
            );
        }
    }
}
