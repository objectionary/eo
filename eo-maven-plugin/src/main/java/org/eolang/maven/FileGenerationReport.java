/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * File generation report.
 * @since 0.56.7
 */
final class FileGenerationReport {

    /**
     * Saved counter.
     */
    private final AtomicInteger counter;

    /**
     * Generated directory.
     */
    private final Path generated;

    /**
     * Target directory.
     */
    private final Path target;

    /**
     * Ctor.
     * @param aggregate Saved counter
     * @param gen Generated directory
     * @param tgt Target directory
     */
    FileGenerationReport(final AtomicInteger aggregate, final Path gen, final Path tgt) {
        this.counter = aggregate;
        this.generated = gen;
        this.target = tgt;
    }

    /**
     * Increment and log the file generation.
     */
    void incrementAndLog() {
        this.counter.incrementAndGet();
        Logger.debug(
            this,
            "Generated %[file]s (%[size]s) file from %[file]s (%[size]s)",
            this.generated, this.generated.toFile().length(),
            this.target, this.target.toFile().length()
        );
    }
}
