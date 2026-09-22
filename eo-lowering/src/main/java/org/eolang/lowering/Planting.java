/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The planting of the entries of the build.
 *
 * <p>A formation is folded by being evaluated, and evaluating it means
 * applying it to something. Its voids are not known at compile time, so
 * each of them is filled with a symbol, a lambda nobody answers, wrapped
 * in the carrier the tables of {@code eo:inference} name for it, and the
 * application is written down as one entry. What the evaluation comes
 * back with is the body of the formation written in terms of its own
 * inputs, which is exactly what a Java method needs to say.</p>
 *
 * <p>A symbol is never planted bare, since a formation holding nothing
 * but a lambda carries no attribute for the body to dispatch off. What
 * the tables cannot type is not planted at all: the body reaches the
 * bottom where it reads it, and the entry is a taint the run records and
 * the later stages leave alone.</p>
 *
 * @since 0.74.0
 * @todo #8548:90min Read every XMIR file of the build with
 *  {@code entries.xsl}, the tables of {@code eo:inference} opened through
 *  {@code document()}, and write three files into the lowering directory.
 *  The first is {@code entries.xmir}, one object named {@code l🌵}
 *  holding the two lambdas of the mark and one binding per formation with
 *  a body: that formation applied to a symbol for each of its voids,
 *  wrapped in {@code mark} with the number of the formation, numbered in
 *  document order across the whole build. The second is
 *  {@code voids.tsv}, one row per symbol, saying the symbol, the number
 *  of its formation, the path by which it is reached from that formation,
 *  and the carrier it was planted in. The third is {@code entries.tsv},
 *  one row per formation, saying its number and its locator, since
 *  nothing else ties a number to the formation it stands for.
 */
final class Planting implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    Planting(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No entry is written yet in %s", this.home);
    }
}
