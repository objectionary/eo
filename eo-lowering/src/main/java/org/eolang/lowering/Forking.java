/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.Arrays;

/**
 * A fire of {@code L_fork}, the {@code if} of a bool the engine answered.
 *
 * <p>When a comparison sees a symbol it cannot answer true or false, so
 * it answers a bool whose {@code if} is this atom, carrying the guard.
 * When that {@code if} fires, phino hands over both arms unevaluated. The
 * fire records the fork, opens the left arm, asks phino for it dataized
 * and stays suspended while phino evaluates it under the same registry,
 * so the rows of the arm land between the row that opens it and the one
 * that closes it with the answer; then the same for the right arm. The
 * carrier of the fork is the carrier of its arms, learned from the
 * answers, and written back into the row that opened it.</p>
 *
 * @since 0.76.0
 */
public final class Forking implements Fire {

    /**
     * The operands of the fire.
     */
    private final Operands args;

    /**
     * The table.
     */
    private final Symbols table;

    /**
     * Ctor.
     *
     * @param operands The operands of the fire
     * @param symbols The table
     */
    public Forking(final Operands operands, final Symbols symbols) {
        this.args = operands;
        this.table = symbols;
    }

    @Override
    public String answer() throws IOException, InterruptedException {
        final String sym = this.table.fresh(
            "object", Arrays.asList("fork", this.args.of("guard", ""))
        );
        String carrier = "";
        for (final String arm : Arrays.asList("left", "right")) {
            this.table.record(sym, arm);
            final String value = this.args.of(arm, "");
            this.table.record(sym, arm, "answer", value);
            if (carrier.isEmpty()) {
                carrier = this.carried(value);
            }
        }
        this.table.record(sym, "end");
        if (carrier.isEmpty()) {
            carrier = "object";
        }
        this.table.retyped(sym, carrier);
        final String out;
        if ("tuple".equals(carrier)) {
            out = new Tuple(sym, this.table).phi();
        } else {
            out = new Marker(String.format("sym:%s", sym), carrier).phi();
        }
        return out;
    }

    /**
     * The carrier of an arm.
     *
     * @param key The answer of the arm
     * @return The carrier, or an empty string when the arm does not say
     */
    private String carried(final String key) {
        final String out;
        if (key.startsWith("sym:")) {
            out = this.table.carrier(key.substring(4));
        } else {
            out = key.substring(0, key.indexOf(':'));
        }
        return out;
    }
}
