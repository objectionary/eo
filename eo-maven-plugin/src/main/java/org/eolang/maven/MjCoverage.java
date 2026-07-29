/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Mojo that produces an LCOV coverage report from raw coverage hits.
 *
 * <p>This goal reads the raw coverage file produced by {@link PhCoverage} at runtime
 * (format: {@code loc:line:pos} per line), merges those hits against the full set of
 * instrumented locations known to the transpiler, and produces an LCOV
 * ({@code .info}) tracefile that Codecov and Coveralls can consume directly.
 * The coverage percentage is also logged.</p>
 *
 * @since 0.58
 * @checkstyle MemberNameCheck (100 lines)
 */
@Mojo(
    name = "coverage",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjCoverage extends MjSafe {

    /**
     * Path to the raw coverage file written by {@link PhCoverage}.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.coverageFile",
        defaultValue = "${project.build.directory}/coverage.txt"
    )
    private Path coverageFile;

    /**
     * Path where the LCOV report will be written.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.lcovFile",
        defaultValue = "${project.build.directory}/coverage.info"
    )
    private Path lcovFile;

    /**
     * Directory containing XMIR files with instrumented locations.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.xmirDir",
        defaultValue = "${project.build.directory}/eo/5-transpile"
    )
    private Path xmirDir;

    @Override
    public void exec() throws IOException {
        if (!Files.exists(this.coverageFile)) {
            Logger.warn(
                this,
                "Coverage file %[file]s not found, skipping LCOV report",
                this.coverageFile
            );
            return;
        }
        final Map<String, Set<Integer>> hits = this.readHits();
        final Map<String, Set<Integer>> all = this.readInstrumented();
        final int total = all.values().stream().mapToInt(Set::size).sum();
        final int covered = hits.values().stream().mapToInt(Set::size).sum();
        final double pct = total > 0 ? 100.0 * covered / total : 0.0;
        this.writeLcov(hits, all);
        Logger.info(
            this,
            "EO coverage: %d / %d instrumented locations hit (%.1f%%) — LCOV report written to %[file]s",
            covered, total, pct, this.lcovFile
        );
    }

    /**
     * Read raw hits from the coverage file.
     * Format: {@code loc:line:pos} per line.
     * @return Map from source file location to set of hit line numbers
     * @throws IOException If fails to read
     */
    private Map<String, Set<Integer>> readHits() throws IOException {
        final Map<String, Set<Integer>> hits = new HashMap<>();
        for (final String line : Files.readAllLines(this.coverageFile, StandardCharsets.UTF_8)) {
            final String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                continue;
            }
            final int first = trimmed.indexOf(':');
            if (first < 0) {
                continue;
            }
            final int second = trimmed.indexOf(':', first + 1);
            if (second < 0) {
                continue;
            }
            final String loc = trimmed.substring(0, first);
            final int lineno;
            try {
                lineno = Integer.parseInt(trimmed.substring(first + 1, second));
            } catch (final NumberFormatException ex) {
                continue;
            }
            hits.computeIfAbsent(loc, k -> new HashSet<>()).add(lineno);
        }
        return hits;
    }

    /**
     * Read all instrumented locations from XMIR files.
     * Scans XMIR files in the transpile output directory for
     * object elements with {@code line} and {@code pos} attributes.
     * @return Map from source file location to set of instrumented line numbers
     * @throws IOException If fails to read
     */
    private Map<String, Set<Integer>> readInstrumented() throws IOException {
        final Map<String, Set<Integer>> all = new HashMap<>();
        if (!Files.isDirectory(this.xmirDir)) {
            Logger.warn(
                this,
                "XMIR directory %[file]s not found, using hit locations as instrumented set",
                this.xmirDir
            );
            return all;
        }
        try (Stream<Path> files = Files.walk(this.xmirDir)) {
            files.filter(path -> path.toString().endsWith(".xmir"))
                .forEach(path -> this.parseXmir(path, all));
        }
        return all;
    }

    /**
     * Parse a single XMIR file and extract instrumented locations.
     * @param path Path to XMIR file
     * @param all Map to populate
     */
    private void parseXmir(final Path path, final Map<String, Set<Integer>> all) {
        try {
            final byte[] content = Files.readAllBytes(path);
            final String text = new String(content, StandardCharsets.UTF_8);
            int idx = 0;
            while (true) {
                final int lineAttr = text.indexOf("line=\"", idx);
                if (lineAttr < 0) {
                    break;
                }
                final int lineEnd = text.indexOf('"', lineAttr + 6);
                if (lineEnd < 0) {
                    break;
                }
                final int lineno;
                try {
                    lineno = Integer.parseInt(text.substring(lineAttr + 6, lineEnd));
                } catch (final NumberFormatException ex) {
                    idx = lineEnd + 1;
                    continue;
                }
                final int posAttr = text.indexOf("pos=\"", lineEnd);
                if (posAttr < 0 || posAttr > lineEnd + 50) {
                    idx = lineEnd + 1;
                    continue;
                }
                final int posEnd = text.indexOf('"', posAttr + 5);
                if (posEnd < 0) {
                    break;
                }
                final String nameAttr = text.indexOf("name=\"", 0, lineAttr) >= 0 ? "" : "";
                final String loc = path.getFileName().toString().replace(".xmir", ".eo");
                all.computeIfAbsent(loc, k -> new HashSet<>()).add(lineno);
                idx = posEnd + 1;
            }
        } catch (final IOException ex) {
            Logger.warn(
                this,
                "Failed to parse XMIR file %[file]s: %s",
                path, ex.getMessage()
            );
        }
    }

    /**
     * Write the LCOV report file.
     * @param hits Map of hit locations
     * @param all Map of all instrumented locations
     * @throws IOException If fails to write
     */
    private void writeLcov(
        final Map<String, Set<Integer>> hits,
        final Map<String, Set<Integer>> all
    ) throws IOException {
        final Set<String> sourceFiles = new HashSet<>();
        sourceFiles.addAll(hits.keySet());
        sourceFiles.addAll(all.keySet());
        final StringBuilder buf = new StringBuilder();
        for (final String source : sourceFiles) {
            buf.append("SF:").append(source).append('\n');
            final Set<Integer> hitLines = hits.getOrDefault(source, new HashSet<>());
            final Set<Integer> allLines = all.getOrDefault(source, new HashSet<>());
            for (final int line : allLines) {
                final int count = hitLines.contains(line) ? 1 : 0;
                buf.append("DA:").append(line).append(',').append(count).append('\n');
            }
            buf.append("end_of_record\n");
        }
        Files.createDirectories(this.lcovFile.getParent());
        Files.write(this.lcovFile, buf.toString().getBytes(StandardCharsets.UTF_8));
    }
}