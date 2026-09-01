/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.inference.Report;

/**
 * The pages a reader opens, one per source file of the program.
 *
 * <p>This one takes the tables as it finds them and draws the author's own
 * source with a mark on every object. The pages land under
 * {@code target/site}, beside the coverage report, and not in
 * {@code target/eo/}, which is the compiler's scratch space.</p>
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
        if (Files.exists(this.tables)) {
            if (Files.exists(this.prepared)) {
                Logger.info(
                    this, "Wrote %d page(s) to look at, they are in %[file]s",
                    new Report(this.prepared, this.tables).written(this.pages), this.pages
                );
            } else {
                Logger.info(
                    this, "The directory %[file]s is absent, nothing to draw from it",
                    this.prepared
                );
            }
        } else {
            Logger.info(
                this, "The directory %[file]s is absent, nothing to draw from it",
                this.tables
            );
        }
    }
}
