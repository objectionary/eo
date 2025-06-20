/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Generated entry.
 * @since 0.56.7
 */
final class GeneratedEntry {

    /**
     * Saved counter.
     */
    private final AtomicInteger saved;

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
     * @param counter Saved counter
     * @param gen Generated directory
     * @param tgt Target directory
     */
    GeneratedEntry(final AtomicInteger counter, final Path gen, final Path tgt) {
        this.saved = counter;
        this.generated = gen;
        this.target = tgt;
    }

    /**
     * Increment it.
     */
    void increment() {
        this.saved.incrementAndGet();
        Logger.debug(
            this,
            "Generated %[file]s (%[size]s) file from %[file]s (%[size]s)",
            this.generated, this.generated.toFile().length(),
            this.target, this.target.toFile().length()
        );
    }
}
