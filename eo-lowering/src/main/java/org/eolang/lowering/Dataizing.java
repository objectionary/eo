/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;

/**
 * A fire of {@code L_dataized}, the atom behind every {@code !} handle.
 *
 * <p>Pure EO reads its arguments through handles, and {@code x!} compiles
 * to {@code dataized(x).as-bytes}, so this atom is where a symbol would
 * otherwise turn into ⊥. It answers the bytes carrier of whatever its
 * target turned out to be: the marker of the symbol when the target is
 * symbolic, the datum when it is not.</p>
 *
 * @since 0.76.0
 */
public final class Dataizing implements Fire {

    /**
     * The operands of the fire.
     */
    private final Operands args;

    /**
     * Ctor.
     *
     * @param operands The operands of the fire
     */
    public Dataizing(final Operands operands) {
        this.args = operands;
    }

    @Override
    public String answer() throws IOException, InterruptedException {
        return new Marker(this.args.of("target", ""), "bytes").phi();
    }
}
