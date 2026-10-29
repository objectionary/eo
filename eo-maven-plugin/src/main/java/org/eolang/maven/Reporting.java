/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Collectors;
import org.eolang.inference.Report;

/**
 * The pages a reader opens, one per source file of the program.
 *
 * <p>This one takes {@code answers.xml} as it finds it in the directory of
 * the tables and draws the author's own source with a mark on every object,
 * coloured by what the engine answers there. The pages land under
 * {@code target/site}, beside the coverage report, and not in
 * {@code target/eo/}, which is the compiler's scratch space. With no
 * {@code answers.xml}, or no prepared XMIR, nothing is drawn and nothing is
 * touched.</p>
 *
 * @since 0.71.0
 */
final class Reporting implements Step {

    /**
     * The directory with the prepared XMIR files.
     */
    private final Path prepared;

    /**
     * The directory with the tables.
     */
    private final Path tables;

    /**
     * The directory for the pages.
     */
    private final Path pages;

    /**
     * Ctor.
     *
     * @param pre The directory with the prepared XMIR files
     * @param rows The directory with the tables
     * @param site The directory for the pages
     */
    Reporting(final Path pre, final Path rows, final Path site) {
        this.prepared = pre;
        this.tables = rows;
        this.pages = site;
    }

    @Override
    public void exec() throws IOException {
        final Path answers = this.tables.resolve("answers.xml");
        if (!Files.exists(answers)) {
            Logger.info(
                this, "The file %[file]s is absent, there is no inference report to draw",
                answers
            );
        } else if (!Files.exists(this.prepared)) {
            Logger.info(
                this, "The directory %[file]s is absent, there is no inference report to draw",
                this.prepared
            );
        } else {
            final Map<String, Integer> tally = new Report(this.prepared, answers)
                .written(this.pages);
            Logger.info(
                this, "The inference report is in %[file]s, with %d object(s) by band: %s",
                this.pages,
                tally.values().stream().mapToInt(Integer::intValue).sum(),
                tally.entrySet().stream()
                    .map(band -> String.format("%s %d", band.getKey(), band.getValue()))
                    .collect(Collectors.joining(", "))
            );
        }
    }
}
