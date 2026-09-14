/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.IOException;
import org.eolang.lowering.Marker;

/**
 * A fire of {@code L_dataized}, the atom behind every {@code !} handle.
 *
 * <p>It takes the operands of the fire and answers the bytes of whatever
 * the target turned out to be: the marker of a symbol when the target is
 * symbolic, the datum when it is not. Without it {@code x!}, which
 * compiles to {@code dataized(x).as-bytes}, would turn every symbol into
 * the terminator.</p>
 *
 * @since 0.76.0
 */
final class Dataizing implements Fire {

    /**
     * The operands of the fire.
     */
    private final Operands args;

    /**
     * Ctor.
     *
     * @param operands The operands of the fire
     */
    Dataizing(final Operands operands) {
        this.args = operands;
    }

    @Override
    public String answer() throws IOException, InterruptedException {
        return new Marker(this.args.of("target", ""), "bytes").phi();
    }
}
