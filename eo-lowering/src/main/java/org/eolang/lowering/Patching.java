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
 * <p>What the binary printed is never read. The patch is made from the
 * XMIR files of the build and the table of symbols alone, so that a
 * formation nothing could be worked out about comes out of the pipeline
 * exactly as its author wrote it, down to the order of its bindings, and
 * the only difference a reader can find in a folded one is the atom that
 * took the place of its body.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Walk the XMIR files of the build with
 *  {@code patch.xsl}, reading {@code symbols.tsv} and {@code entries.tsv}
 *  through {@code unparsed-text()}, and write the result into the
 *  {@code patched/} directory under the lowering directory. A formation
 *  whose number has a {@code root} row gets a new attribute, an atom named
 *  after that number, and a phi that dispatches to it, while every other
 *  binding of it, the voids and the nested formations and the tests, stays
 *  where it was, and a formation without such a row changes not at all.
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
