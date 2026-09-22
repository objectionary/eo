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
 * statements here, one per firing the protocol holds, and the atom the
 * patch put into the formation is the class those statements live in. The
 * firings are read in the order they opened and nothing is rearranged,
 * since that order is the one the calculus worked them out in.</p>
 *
 * <p>The protocol is read and the program phino morphed is not, because
 * the protocol already says what fired, in what order, and off which
 * symbol, which is all a Java method is; reading the morphed program back
 * would mean parsing a phi-expression, and this module parses none.</p>
 *
 * @since 0.74.0
 * @todo #8548:90min Render one Java class per rooted entry of
 *  {@code protocol.xml} into the directory of generated sources, naming
 *  it by the rule {@code _java-names.xsl} gives every atom attribute of
 *  the transpiler. Find the formation of a number through
 *  {@code entries.tsv}, read each void symbol as a chain of {@code take}
 *  calls along the path {@code voids.tsv} gives it, spell every firing
 *  reachable from the root as one statement under its lambda name, a fork
 *  as an {@code if} that assigns a blank final, and return the root.
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
