/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * An LCOV tracefile built from a {@link CoverageManifest} (every location the
 * transpiler wrapped into {@code PhCoverage}) and the raw hits {@code
 * PhCoverage} recorded at runtime, one {@code loc:line:pos} line per touched
 * location. A manifest line whose locator never shows up among the hits is a
 * line that was never dataized; everything else counts as covered.
 * @since 0.58
 */
final class CoverageReport {

    /**
     * Matches one {@code loc:line:pos} hit line.
     */
    private static final Pattern HIT = Pattern.compile("^(.*):(\\d+):(\\d+)$");

    /**
     * The manifest of all instrumented locations.
     */
    private final Path manifest;

    /**
     * The raw hits file, may not exist yet if nothing was ever touched.
     */
    private final Path hits;

    /**
     * Resolves a location's locator to the source it belongs to.
     */
    private final ObjectSources sources;

    /**
     * Ctor.
     * @param locations The manifest of all instrumented locations
     * @param touched The raw hits file
     * @param registered Resolves a location's locator to the source it belongs to
     */
    CoverageReport(final Path locations, final Path touched, final ObjectSources registered) {
        this.manifest = locations;
        this.hits = touched;
        this.sources = registered;
    }

    /**
     * The LCOV tracefile text, one {@code SF}/{@code DA}/{@code LF}/
     * {@code LH}/{@code end_of_record} block per source file, sorted by
     * file path for a deterministic result.
     * @return The LCOV text
     * @throws IOException If fails to read the manifest or the hits file
     */
    String text() throws IOException {
        final Set<String> touched = this.touched();
        final SortedMap<String, SortedMap<Integer, Boolean>> files = new TreeMap<>();
        for (final String line : Files.readAllLines(this.manifest)) {
            if (line.isEmpty()) {
                continue;
            }
            final String[] parts = line.split("\t", 2);
            final String loc = parts[0];
            final int number = Integer.parseInt(parts[1]);
            files.computeIfAbsent(this.sources.source(loc), ignored -> new TreeMap<>())
                .merge(number, touched.contains(loc), (was, now) -> was || now);
        }
        final StringBuilder text = new StringBuilder(files.size() * 64);
        for (final Map.Entry<String, SortedMap<Integer, Boolean>> file : files.entrySet()) {
            text.append(String.format("SF:%s%n", file.getKey()));
            int hit = 0;
            for (final Map.Entry<Integer, Boolean> line : file.getValue().entrySet()) {
                final int mark;
                if (line.getValue()) {
                    mark = 1;
                } else {
                    mark = 0;
                }
                hit += mark;
                text.append(String.format("DA:%d,%d%n", line.getKey(), mark));
            }
            text.append(String.format("LF:%d%n", file.getValue().size()))
                .append(String.format("LH:%d%n", hit))
                .append("end_of_record").append(System.lineSeparator());
        }
        return text.toString();
    }

    /**
     * The percentage of instrumented lines that were touched, derived from
     * {@link #text()} itself so there is exactly one place that knows the
     * LCOV grammar.
     * @return The percentage, from 0 to 100; 100 when nothing is instrumented
     * @throws IOException If fails to read the manifest or the hits file
     */
    double percentage() throws IOException {
        int total = 0;
        int covered = 0;
        for (final String line : this.text().split(System.lineSeparator())) {
            if (line.startsWith("LF:")) {
                total += Integer.parseInt(line.substring("LF:".length()));
            } else if (line.startsWith("LH:")) {
                covered += Integer.parseInt(line.substring("LH:".length()));
            }
        }
        final double result;
        if (total == 0) {
            result = 100.0d;
        } else {
            result = covered * 100.0d / total;
        }
        return result;
    }

    /**
     * The set of locators that were touched at least once.
     * @return The locators, empty when the hits file does not exist yet
     * @throws IOException If fails to read the hits file
     */
    private Set<String> touched() throws IOException {
        final Set<String> result = new HashSet<>(0);
        if (Files.exists(this.hits)) {
            for (final String line : Files.readAllLines(this.hits)) {
                if (line.isEmpty()) {
                    continue;
                }
                final Matcher matcher = CoverageReport.HIT.matcher(line);
                if (matcher.matches()) {
                    result.add(matcher.group(1));
                }
            }
        }
        return result;
    }
}
