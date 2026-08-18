/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Turns the raw {@code loc:line:pos} coverage hits {@code PhCoverage}
 * appended at runtime into an LCOV tracefile.
 *
 * <p>Bound to the {@code verify} phase, after the tests that actually
 * populate the raw hits file have run (unlike {@code transpile}, which
 * only decides whether locations get wrapped in {@code PhCoverage} in
 * the first place, long before any test executes). Silently does
 * nothing when {@code eo.coverageFile} is not set or the file does not
 * exist yet, the same "no-op when disabled" contract {@code PhCoverage}
 * itself follows.</p>
 *
 * @since 0.75.0
 */
@Mojo(
    name = "coverage-report",
    defaultPhase = LifecyclePhase.VERIFY,
    threadSafe = true
)
public final class MjCoverageReport extends MjSafe {

    /**
     * The raw {@code loc:line:pos} hits file {@code PhCoverage} appended to.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.coverageFile")
    private File coverageFile;

    /**
     * Where to write the LCOV tracefile.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.lcovFile",
        defaultValue = "${project.build.directory}/coverage.info"
    )
    private File lcovFile;

    /**
     * The minimum percentage of dataized {@code .eo} objects that must be
     * covered, or the build fails. Zero by default, so a build with
     * coverage tracking off (the common case, see {@code coverageTracking}
     * on {@link MjTranspile}) never fails on a threshold it never opted
     * into; {@code eo-runtime} raises it once tracking is on, mirroring
     * how the {@code jacoco} profile binds a {@code check} goal with its
     * own per-metric thresholds.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.minCoverage", defaultValue = "0")
    private double minCoverage;

    @Override
    public void exec() throws IOException {
        if (this.coverageFile == null || !this.coverageFile.exists()) {
            Logger.info(
                this,
                "No coverage hits file at %[file]s, skipping the LCOV report",
                this.coverageFile
            );
        } else {
            this.report();
        }
    }

    /**
     * Build the manifest of every instrumented location, match it against
     * the raw hits, save the LCOV tracefile, and enforce the minimum
     * coverage threshold.
     * @throws IOException If reading the hits file or writing the report
     *  fails, or the covered percentage is below {@link #minCoverage}
     */
    private void report() throws IOException {
        final Map<String, Map<Integer, Integer>> perfile = new LinkedHashMap<>(0);
        final Map<String, Integer> lineof = new HashMap<>(0);
        final Map<String, String> fileof = new HashMap<>(0);
        final CoverageManifest manifest = new CoverageManifest();
        try (TjsForeign tojos = this.tojos()) {
            for (final TjForeign tojo : tojos.standalone()) {
                final String source = tojo.source().toString();
                final XML xmir = new XMLDocument(tojo.xmir());
                for (final String location : manifest.locations(xmir)) {
                    final int last = location.lastIndexOf(':');
                    final int line = Integer.parseInt(
                        location.substring(location.lastIndexOf(':', last - 1) + 1, last)
                    );
                    perfile.computeIfAbsent(source, key -> new LinkedHashMap<>(0))
                        .putIfAbsent(line, 0);
                    lineof.put(location, line);
                    fileof.put(location, source);
                }
            }
        }
        final List<String> hits = Files.readAllLines(this.coverageFile.toPath());
        for (final String hit : hits) {
            final String source = fileof.get(hit);
            if (source != null) {
                perfile.get(source).merge(lineof.get(hit), 1, Integer::sum);
            }
        }
        final LcovReport report = new LcovReport(perfile);
        new Saved(report.text(), this.lcovFile.toPath()).value();
        final double covered = report.covered();
        Logger.info(
            this, "EO object coverage: %.1f%%, LCOV report saved to %[file]s",
            covered, this.lcovFile
        );
        if (covered < this.minCoverage) {
            throw new IOException(
                String.format(
                    "EO object coverage is %.1f%%, below the required %.1f%% minimum",
                    covered, this.minCoverage
                )
            );
        }
    }
}
