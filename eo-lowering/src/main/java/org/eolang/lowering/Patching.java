/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The putting of an atom in the place of every folded body.
 *
 * <p>The program phino morphed is never read. The patch is made from the
 * XMIR files of the build and the protocol of the run alone, so that a
 * formation nothing could be worked out about comes out of the pipeline
 * exactly as its author wrote it, down to the order of its bindings, and
 * the only difference a reader can find in a folded one is the atom that
 * took the place of its body.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Walk the XMIR files of the build with
 *  {@code patch.xsl}, reading {@code entries.tsv} through
 *  {@code unparsed-text()} and {@code protocol.xml} through
 *  {@code document()}, and write the result into the {@code patched/}
 *  directory under the lowering directory. A formation whose entry came
 *  back with a root gets one attribute more, an atom named
 *  {@code l🌵<n>} after the number of that entry, and its {@code φ}
 *  becomes {@code ξ.l🌵<n>}, while every other binding of it, the voids
 *  and the nested formations and the tests and what a package object
 *  holds, stays where it was. A tainted formation changes not at all.
 */
final class Patching implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    Patching(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No file is patched yet in %s", this.home);
    }
}
