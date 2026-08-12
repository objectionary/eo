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
import java.util.Collections;
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
 * {@code SF:} path under the directory of {@code .eo} sources, slashed
 * the same way on every platform, and a line becomes a {@code DA:}
 * counter of how many objects of that line were touched. It runs after
 * the tests and needs {@code coverageTracking} on the {@code transpile}
 * goal, since nothing is instrumented otherwise.
 * A torn record is skipped, since the tests append concurrently.</p>
 *
 * @since 0.74.0
 */
@Mojo(name = "lcov", defaultPhase = LifecyclePhase.VERIFY, threadSafe = true)
public final class MjLcov extends MjSafe {

    /**
     * The file where {@code PhCoverage} appends one record per touched object,
     * named to the tests through the {@code eo.coverageFile} property.
     * @todo #5466:60min Report the objects the run never touched. The tracefile
     *  names only the lines the tests reached, so {@code LH} equals {@code LF}
     *  and every reader sees a hundred per cent. Merge these hits against the
     *  locations the transpiler instrumented, so an untouched line is reported
     *  as {@code DA:n,0}. Let {@code to-java.xsl} write those out beside the
     *  generated Java, since it already decides them while it emits the
     *  wrappers. Truncate this file then too, since nothing does and a second
     *  {@code verify} without a {@code clean} reports what both runs touched.
     */
    @Parameter(
        property = "eo.coverageFile",
        defaultValue = "${project.build.directory}/coverage.txt"
    )
    private File hits;

    @Override
    void exec() throws IOException {
        final Path saved = this.targetDir.toPath().resolve("eo-lcov.info");
        Files.createDirectories(saved.getParent());
        Files.write(saved, this.lcov().getBytes(StandardCharsets.UTF_8));
        Logger.info(this, "EO object coverage saved to %[file]s", saved);
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
        final Iterable<String> records;
        if (Files.exists(path)) {
            records = new SetOf<>(Files.readAllLines(path));
        } else {
            records = Collections.emptySet();
        }
        final Map<String, Map<Integer, Integer>> counts = new TreeMap<>();
        for (final String record : records) {
            final String[] parts = record.split(":", 3);
            if (parts.length == 3 && parts[1].matches("\\d+")) {
                counts.computeIfAbsent(parts[0], program -> new TreeMap<>())
                    .merge(Integer.valueOf(parts[1]), 1, Integer::sum);
            } else {
                Logger.warn(this, "The coverage record '%s' is skipped", record);
            }
        }
        return counts;
    }
}
