/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;

/**
 * Where a transpile writes its Java output, and the report of it.
 * @since 0.77
 */
final class Written {

    /**
     * Generated sources directory.
     */
    private final Path generated;

    /**
     * Whether to transpile tests.
     */
    private final boolean tests;

    /**
     * Directories with the Java sources a human wrote.
     */
    private final Collection<Path> roots;

    /**
     * Ctor.
     * @param generated Generated sources directory
     * @param tests Whether to transpile tests
     * @param roots Directories with the Java sources a human wrote
     */
    Written(final Path generated, final boolean tests, final Collection<Path> roots) {
        this.generated = generated;
        this.tests = tests;
        this.roots = roots;
    }

    /**
     * Java files this run writes into.
     * @return The files
     */
    JavaFiles files() {
        return new JavaFiles(this.generated);
    }

    /**
     * Whether a tojo transpiles into test sources.
     * @param tojo The tojo
     * @return True if it does
     */
    boolean tests(final TjForeign tojo) {
        return this.tests && !tojo.discovered();
    }

    /**
     * Log how many Java files this run wrote, creating package-info
     * files for the directories it touched.
     * @param transpiled Amount of Java files created directly
     * @param sources Amount of XMIRs given to transpile
     * @param files The Java files this run wrote
     * @throws IOException If fails to create a package-info file
     */
    void log(final int transpiled, final int sources, final JavaFiles files) throws IOException {
        Logger.info(
            this, "Transpiled %d XMIRs, created %d Java files in %[file]s",
            sources,
            transpiled + new PackageInfos(
                this.generated, this.roots, files.directories()
            ).create(),
            this.generated
        );
    }
}
