/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The one running of phino over the world.
 *
 * <p>Every formation of the build is folded by a single call of the
 * binary, which walks the entries one application after another and asks
 * the atom engine to serve each lambda it fires. One run rather than one
 * per formation, because the world is parsed once and a formation that
 * calls another is then folded with the answer of that other one already
 * at hand.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Write {@code atoms.json} and the one-line launcher it
 *  points at, then run {@code phino morph --deep --partial
 *  --atoms=atoms.json --max-steps=1000000 --inside='Φ.l🌵' --hide-rho
 *  world.phi} with its stdout and its stderr sent to files under the
 *  lowering directory. Fail the build when the binary comes back with an
 *  exit code other than zero, when the run reaches the step limit, when a
 *  line arriving on the channel of the engine does not parse, or when a
 *  box ends the run with neither a {@code root} row nor a {@code taint}
 *  row of its own in {@code symbols.tsv}.
 */
final class Running implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    Running(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No world is evaluated yet in %s", this.home);
    }
}
