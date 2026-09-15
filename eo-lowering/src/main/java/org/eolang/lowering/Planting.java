/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The planting of a marker in every void of every boxed formation.
 *
 * <p>A formation is folded by being evaluated, and evaluating it means
 * applying it to something. Its voids are not known at compile time, so
 * each of them is filled with a marker, a lambda nobody serves, which the
 * calculus leaves standing wherever it lands. What comes back is the body
 * of the formation written in terms of its own inputs, which is exactly
 * what a Java method needs to say.</p>
 *
 * @since 0.74.0
 * @todo #8548:90min Read the boxed files with {@code entries.xsl}, the
 *  tables of {@code eo:inference} imported through {@code document()}, and
 *  write two files into the lowering directory. The first is
 *  {@code entries.xmir}, a single object named {@code l🌵} holding one
 *  application per boxed formation, over a marker {@code V_<n>_<k>} for
 *  each void of it, wrapped in the carrier the tables give that void. The
 *  second is {@code voids.tsv}, one row per marker, saying the name of the
 *  marker, the path by which it is reached from the formation, and the
 *  carrier it was planted in.
 */
public final class Planting implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    public Planting(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No entry is written yet in %s", this.home);
    }
}
