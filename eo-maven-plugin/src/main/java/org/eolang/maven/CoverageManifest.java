/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

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
     * Selects exactly the elements that end up wrapped into {@code
     * PhCoverage}. This is the same {@code @line}/{@code @pos}/{@code @loc}
     * condition the final {@code to-java.xsl} shift itself tests, applied
     * instead to the XMIR right before that shift runs, so the manifest is
     * derived from that structure directly, not by pattern-matching the
     * Java text {@code to-java.xsl} emits.
     */
    private static final String LOCATED =
        "//*[@line and @pos and not(contains(@loc, '+'))]";

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
     * Scan the located (post-locators, pre-{@code to-java.xsl}) XMIR for the
     * elements that will be wrapped into {@code PhCoverage}, and append one
     * manifest line per location found, in a single write. Just like
     * {@code PhCoverage} itself, this relies on the OS append guarantee for
     * a single {@code write()} syscall to stay safe across the transpiler's
     * worker threads, with no explicit locking.
     * @param located The XMIR, after every transpile shift except the final
     *  {@code to-java.xsl} one
     * @throws IOException If fails to append
     */
    void scan(final XML located) throws IOException {
        final List<String> lines = new ArrayList<>(0);
        for (final XML element : located.nodes(CoverageManifest.LOCATED)) {
            lines.add(
                String.format(
                    "%s\t%s%n",
                    element.xpath("@loc").get(0),
                    element.xpath("@line").get(0)
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
