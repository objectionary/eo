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
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;

/**
 * Save an LCOV tracefile of the EO objects the tests touched.
 *
 * <p>It reads the {@code program:line:pos} records {@code PhCoverage}
 * appends while the tests run and saves them as LCOV, which Codecov,
 * Coveralls and {@code genhtml} read as is: a program becomes an
 * {@code SF:} path under the {@code .eo} sources, slashed the same way
 * on every platform, and a line becomes a {@code DA:} counter of the
 * objects touched on it. It needs {@code coverageTracking} on the
 * {@code transpile} goal, since nothing is instrumented otherwise, and
 * skips a torn record, since the tests append concurrently.</p>
 *
 * @since 0.74.0
 */
@Mojo(name = "lcov", defaultPhase = LifecyclePhase.VERIFY, threadSafe = true)
public final class MjLcov extends MjSafe {

    /**
     * The file where {@code PhCoverage} appends one record per touched object,
     * named to the tests through the {@code eo.coverageFile} property.
     * @todo #5466:60min Report the objects the run never touched. Only the
     *  lines the tests reached are named, so {@code LH} equals {@code LF} and
     *  every reader sees a hundred per cent. Merge these hits against the
     *  locations {@code to-java.xsl} instrumented, which it can write out
     *  beside the generated Java, and report an untouched line as
     *  {@code DA:n,0}. Truncate this file then too, since nothing does.
     */
    @Parameter(
        property = "eo.coverageFile",
        defaultValue = "${project.build.directory}/coverage.txt"
    )
    private File hits;

    /**
     * The LCOV tracefile to save, named through the {@code eo.lcovFile}
     * property, so a build may put it where its coverage tool looks.
     */
    @Parameter(
        property = "eo.lcovFile",
        defaultValue = "${project.build.directory}/eo/eo-lcov.info"
    )
    private File tracefile;

    @Override
    void exec() throws IOException {
        final Path saved = this.tracefile.toPath();
        Files.createDirectories(saved.getParent());
        Files.write(saved, this.lcov().getBytes(StandardCharsets.UTF_8));
        Logger.info(
            this, "EO object coverage saved to %[file]s (%[size]s)", saved, saved.toFile().length()
        );
    }

    /**
     * The tracefile of everything the run touched.
     * @return LCOV text, empty when nothing was recorded
     * @throws IOException If fails to read the records
     */
    private String lcov() throws IOException {
        return this.counted().entrySet().stream().map(
            program -> String.format(
                "TN:%nSF:%s%n%sLF:%d%nLH:%d%nend_of_record%n",
                this.sourcesDir.toPath().resolve(
                    String.format("%s.eo", program.getKey().replace('.', '/'))
                ).toString().replace(File.separatorChar, '/'),
                program.getValue().entrySet().stream().map(
                    line -> String.format("DA:%d,%d%n", line.getKey(), line.getValue())
                ).collect(Collectors.joining()),
                program.getValue().size(),
                program.getValue().size()
            )
        ).collect(Collectors.joining());
    }

    /**
     * How many objects of each line of each program the run touched.
     * @return Programs in alphabetical order, each with its lines in order
     * @throws IOException If fails to read the records
     */
    private Map<String, Map<Integer, Integer>> counted() throws IOException {
        final Path path = this.hits.toPath();
        final Map<String, Map<Integer, Integer>> counts = new TreeMap<>();
        if (Files.exists(path)) {
            for (final String record : new SetOf<>(Files.readAllLines(path))) {
                final String[] parts = record.split(":", 3);
                if (parts.length == 3 && parts[1].matches("\\d+")) {
                    counts.computeIfAbsent(parts[0], program -> new TreeMap<>())
                        .merge(Integer.valueOf(parts[1]), 1, Integer::sum);
                } else {
                    Logger.warn(this, "The coverage record '%s' is skipped", record);
                }
            }
        }
        return counts;
    }
}
