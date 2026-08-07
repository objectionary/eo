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
import java.util.Collection;
import java.util.Collections;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Save an LCOV tracefile of EO object coverage.
 * <p>
 *     This Maven Mojo reads the hits {@code PhCoverage} appended while the
 *     tests ran and saves them as LCOV. It runs after the tests, so it is
 *     bound to {@code verify}, and it needs {@code coverageTracking} on
 *     the {@code transpile} goal, since otherwise nothing is instrumented
 *     and no hit is ever recorded.
 * </p>
 * @since 0.58
 */
@Mojo(
    name = "lcov",
    defaultPhase = LifecyclePhase.VERIFY,
    threadSafe = true
)
public final class MjLcov extends MjSafe {

    /**
     * The file where {@code PhCoverage} appended one record per touched
     * object, named by the {@code eo.coverageFile} system property of the
     * JVM that ran the tests.
     * @todo #5466:60min Report the objects the run never touched.
     *  The tracefile now lists only the lines the tests reached, so every
     *  line it names is covered and {@code LH} always equals {@code LF}.
     *  Merge these hits against every location the transpiler
     *  instrumented, so an untouched line is reported as {@code DA:n,0}
     *  and the percentage means something. Those locations are readable
     *  back out of the generated Java, where each of them is a
     *  {@code new PhCoverage(..)} wrapper naming its program, line and
     *  position, which is the same set {@code to-java.xsl} decided on
     *  while emitting them and survives on disk between this goal and
     *  {@code transpile}, two separate Maven executions.
     */
    @Parameter(
        property = "eo.coverageFile",
        defaultValue = "${project.build.directory}/coverage.txt"
    )
    private File hits;

    /**
     * Where to save the LCOV tracefile.
     * @todo #5466:30min Bind this goal in {@code eo-runtime/pom.xml}.
     *  Surefire there already names {@code eo.coverageFile}, but nothing
     *  runs {@code lcov} afterwards, so the project that records the hits
     *  never turns them into a report. Add an execution binding this goal
     *  to {@code verify}, next to the {@code compile} one, once
     *  {@code coverageTracking} is on and the puzzle above is done.
     */
    @Parameter(
        property = "eo.lcovFile",
        defaultValue = "${project.build.directory}/eo-lcov.info"
    )
    private File tracefile;

    /**
     * Ctor.
     */
    public MjLcov() {
        // nothing
    }

    @Override
    public void exec() throws IOException {
        final Path target = this.tracefile.toPath();
        Files.createDirectories(target.getParent());
        Files.write(
            target,
            new Lcov(this.sourcesDir.toPath(), this.recorded())
                .toString().getBytes(StandardCharsets.UTF_8)
        );
        Logger.info(this, "EO object coverage saved to %[file]s", target);
    }

    /**
     * Every location the run touched.
     * @return The records, or nothing when no test recorded any
     * @throws IOException If fails to read them
     */
    private Collection<String> recorded() throws IOException {
        final Path path = this.hits.toPath();
        final Collection<String> records;
        if (Files.exists(path)) {
            records = Files.readAllLines(path, StandardCharsets.UTF_8);
        } else {
            records = Collections.emptyList();
        }
        return records;
    }
}
