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
 * binary. The call is aimed at the entries, it enters each of them in the
 * order they were written, and the protocol it writes beside the world is
 * the only thing the stages after this one read. One run rather than one
 * per formation, because the world is parsed once and a formation that
 * calls another is folded with the arithmetic of that other one already
 * unfolded into it.</p>
 *
 * <p>What the binary can say about a primitive is said in
 * {@code atoms.yaml} and nowhere else. A lambda no entry of that file
 * matches is left standing where it is, and the formation that reached it
 * is a taint: nothing is guessed about it, and nothing is folded.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Write {@code atoms.yaml} beside the world, one entry
 *  per primitive lambda the renderer can spell in Java, plus
 *  {@code L_fork}, {@code L_entry} and {@code L_root}, exactly as the
 *  YAML of the issue has them, and then make the one run:
 *  {@code phino morph --deep --symbolic=atoms.yaml --locator='Q.l🌵'
 *  --protocol=protocol.xml --acyclic --partial --quiet
 *  --max-steps=<bounded> world.phi}. Fail the build when the binary comes
 *  back with an exit code other than zero, the run that reached the step
 *  ceiling among them. Nothing is retried and nothing is skipped: a
 *  formation the run parks is a taint that the patch and the renderer
 *  read off the protocol and leave alone.
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
