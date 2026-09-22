/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The writing of the Java the folded formations became.
 *
 * <p>This is where the work of the pipeline is paid back. A body that was
 * an object graph built and dataized at runtime is a handful of Java
 * statements here, one per row of the table, and the atom the patch put
 * into the formation is the class those statements live in. The rows are
 * read in order and nothing is rearranged, since the table already holds
 * them in the order the calculus worked them out.</p>
 *
 * @since 0.74.0
 * @todo #8548:90min Render one Java class per {@code root} row of
 *  {@code symbols.tsv} into the directory of generated sources, naming it
 *  by the same rule {@code _java-names.xsl} uses for the atom attributes
 *  of the transpiler. Find the formation of a number through
 *  {@code entries.tsv}, read each void of it along the path
 *  {@code voids.tsv} gives that marker, and spell the operation rows, the
 *  fork rows, and the fail rows of the table as the Java statements of the
 *  body.
 */
final class Rendering implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    Rendering(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No Java is rendered yet from %s", this.home);
    }
}
