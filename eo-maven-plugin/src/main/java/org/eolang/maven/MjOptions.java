/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Set;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;

/**
 * Shared Maven options for binary placement and removal.
 *
 * @since 0.64.0
 */
public abstract class MjOptions extends MjSafe {

    /**
     * Filters for binary placement and removal.
     */
    private BinaryFilters binaries;

    /**
     * Ctor.
     */
    protected MjOptions() {
        this.binaries = new BinaryFilters(
            new SetOf<>("**"), new SetOf<>(), new SetOf<>()
        );
    }

    /**
     * List of inclusion GLOB filters for finding class files while placing them from where
     * they were resolved to classes directory.
     *
     * @param globs Glob patterns
     * @since 0.15
     */
    @Parameter
    public final void setPlaceBinaries(final Set<String> globs) {
        this.binaries = new BinaryFilters(
            globs, this.binaries.excludes(), this.binaries.keeps()
        );
    }

    /**
     * List of exclusion GLOB filters for finding class files while placing them from where
     * they were resolved to classed directory.
     *
     * @param globs Glob patterns
     * @since 0.15
     */
    @Parameter
    public final void setSkipBinaries(final Set<String> globs) {
        this.binaries = new BinaryFilters(
            this.binaries.includes(), globs, this.binaries.keeps()
        );
    }

    /**
     * List of inclusion GLOB filters for unplacing and unspiling (ONLY these files will stay).
     *
     * @param globs Glob patterns
     * @see <a href="https://news.eolang.org/2022-07-15-placing-and-unplacing.html">Placing and Unplacing in JAR Artifacts</a>
     * @since 0.24
     */
    @Parameter
    public final void setKeepBinaries(final Set<String> globs) {
        this.binaries = new BinaryFilters(
            this.binaries.includes(), this.binaries.excludes(), globs
        );
    }

    /**
     * Current placement filters.
     *
     * @return Filters set for this goal
     */
    final BinaryFilters binaries() {
        return this.binaries;
    }
}
