/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.parser.EoSyntax;

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
 * <p>Every {@code .eo} file is re-parsed from its own source instead of
 * reading the {@code xmir} stored in its tojo, because that xmir is the
 * {@code MjMerge} one for a package root and mixes the members it spliced
 * in, whose lines refer to their own files. Reporting those members'
 * locations against the package root puts lines of unrelated files where
 * the package's meta directives are, so each file is covered under its
 * own name (they all carry {@code @loc} with the {@code Φ} prefix of the
 * object they name, so the runtime hits still line up).</p>
 *
 * <p>Every registered {@code .eo} file gets a record of its own, even one
 * the manifest names no location in: a file of nothing but atom
 * declarations and unit tests, like {@code bytes.eo}, has no dataizable
 * body at all. Such a file used to be absent from the tracefile
 * altogether, which left the report unable to tell it apart from a file
 * that was never compiled (#7057).</p>
 *
 * <p>The raw hits are read one line at a time and never held together in
 * memory. What the report keeps out of them is one counter per line of
 * source, so its own size is bounded by the manifest, but the file the
 * runtime appends to is not: every {@code PhCoverage} wrapper starts with
 * a dedup set of its own, so a location touched through many instances of
 * the same object is appended once per instance (#6508), and a whole test
 * suite leaves gigabytes of duplicates behind for the few thousand
 * distinct locations in them. Reading that file into a list first is what
 * killed the {@code eo-runtime} coverage build with an
 * {@code OutOfMemoryError} once it outgrew the 4Gb heap
 * {@code .mvn/jvm.config} gives Maven.</p>
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
     */
    @Parameter(alias = "coverageFile", property = "eo.coverageFile")
    private File hits;

    /**
     * Where to write the LCOV tracefile.
     */
    @Parameter(
        alias = "lcovFile",
        property = "eo.lcovFile",
        defaultValue = "${project.build.directory}/coverage.info"
    )
    private File lcov;

    /**
     * The minimum percentage of dataized {@code .eo} objects that must be
     * covered, or the build fails. Zero by default, so a build with
     * coverage tracking off (the common case, see {@code coverageTracking}
     * on {@link MjTranspile}) never fails on a threshold it never opted
     * into; {@code eo-runtime} raises it once tracking is on, mirroring
     * how the {@code jacoco} profile binds a {@code check} goal with its
     * own per-metric thresholds.
     */
    @Parameter(alias = "minCoverage", property = "eo.minCoverage", defaultValue = "0")
    private double minimum;

    /**
     * Ctor.
     */
    public MjCoverageReport() {
        // nothing
    }

    @Override
    public void exec() throws IOException {
        if (this.hits == null || !this.hits.exists()) {
            Logger.info(
                this,
                "No coverage hits file at %[file]s, skipping the LCOV report",
                this.hits
            );
        } else {
            this.report();
        }
    }

    private void report() throws IOException {
        final Map<String, Map<Integer, Integer>> perfile = new LinkedHashMap<>(0);
        final Map<String, Integer> lineof = new HashMap<>(0);
        final Map<String, String> fileof = new HashMap<>(0);
        final CoverageManifest manifest = new CoverageManifest();
        try (TjsForeign tojos = this.tojos()) {
            for (final TjForeign tojo : tojos.withXmir()) {
                final String source = tojo.source().toString();
                final Map<Integer, Integer> lines = perfile.computeIfAbsent(
                    source, key -> new LinkedHashMap<>(0)
                );
                final XML xmir = new EoSyntax(Files.readString(tojo.source())).parsed();
                if (!xmir.nodes("/object/errors/error").isEmpty()) {
                    throw new IOException(
                        String.format(
                            "Can't build a coverage report, %s doesn't parse", source
                        )
                    );
                }
                for (final String location : manifest.locations(xmir)) {
                    final int last = location.lastIndexOf(':');
                    final int line = Integer.parseInt(
                        location.substring(location.lastIndexOf(':', last - 1) + 1, last)
                    );
                    lines.putIfAbsent(line, 0);
                    lineof.put(location, line);
                    fileof.put(location, source);
                }
            }
        }
        try (Stream<String> records = Files.lines(this.hits.toPath())) {
            records.forEach(
                hit -> {
                    final String source = fileof.get(hit);
                    if (source != null) {
                        perfile.get(source).merge(lineof.get(hit), 1, Integer::sum);
                    }
                }
            );
        }
        final LcovReport report = new LcovReport(perfile);
        new Saved(report.text(), this.lcov.toPath()).value();
        final double covered = report.covered();
        Logger.info(
            this, "EO object coverage: %.1f%%, LCOV report saved to %[file]s",
            covered, this.lcov
        );
        if (covered < this.minimum) {
            throw new IOException(
                String.format(
                    "EO object coverage is %.1f%%, below the required %.1f%% minimum",
                    covered, this.minimum
                )
            );
        }
    }
}
