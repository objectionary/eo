/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Turns the raw {@code PhCoverage} hits recorded by {@code eo.coverageFile}
 * into an LCOV tracefile, merging them against the manifest of all
 * instrumented locations that {@link MjTranspile} wrote alongside that same
 * file (see {@link CoverageManifest}). Each location's source file is
 * resolved from the {@code eo-foreign} catalog that {@code register}
 * already builds, matched by the object id at the front of its locator
 * (see {@link ObjectSources}).
 * <p>
 *     This goal has to run after the tests that actually touch the
 *     instrumented objects, so it belongs late in the lifecycle (the
 *     {@code verify} phase by default), well after {@code transpile}.
 * </p>
 * @since 0.58
 */
@Mojo(
    name = "coverage-report",
    defaultPhase = LifecyclePhase.VERIFY,
    threadSafe = true
)
public final class MjCoverageReport extends MjSafe {

    /**
     * The same raw coverage hits file passed to the {@code transpile} goal.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.coverageFile", required = true)
    private File coverageFile;

    /**
     * Where to write the LCOV tracefile. Defaults to a {@code coverage.info}
     * file next to {@link #coverageFile}.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.coverageReport")
    private File coverageReport;

    @Override
    void exec() throws IOException {
        final Path hits = this.coverageFile.toPath();
        final Path manifest = new CoverageManifest(hits).path();
        if (!Files.exists(manifest)) {
            throw new IOException(
                String.format(
                    "No coverage manifest found at %s; was this project transpiled with eo.coverageFile set to %s?",
                    manifest, hits
                )
            );
        }
        final CoverageReport report = new CoverageReport(
            manifest, hits, new ObjectSources(this.registered())
        );
        final Path target = this.target();
        Files.write(target, report.text().getBytes(StandardCharsets.UTF_8));
        Logger.info(
            this, "Coverage report written to %[file]s: %.2f%% of instrumented lines covered",
            target, report.percentage()
        );
    }

    /**
     * Every registered object id mapped to the source it was compiled from,
     * relative to the project's basedir, as already tracked by the
     * {@code eo-foreign} catalog.
     * @return Object id mapped to its source
     */
    private Map<String, String> registered() {
        final Path base = this.project.getBasedir().toPath();
        final Map<String, String> ids = new HashMap<>(0);
        for (final TjForeign tojo : this.scopedTojos().all()) {
            ids.put(
                tojo.identifier(),
                base.relativize(tojo.source().toAbsolutePath()).toString()
            );
        }
        return ids;
    }

    /**
     * Where to write the LCOV tracefile.
     * @return The path, defaulting to a {@code coverage.info} sibling of
     *  {@link #coverageFile} when {@link #coverageReport} is not set
     */
    private Path target() {
        final Path result;
        if (this.coverageReport == null) {
            result = this.coverageFile.toPath().resolveSibling("coverage.info");
        } else {
            result = this.coverageReport.toPath();
        }
        return result;
    }
}
