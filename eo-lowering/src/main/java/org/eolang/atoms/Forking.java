/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.eolang.lowering.Marker;
import org.eolang.lowering.Symbols;
import org.eolang.lowering.Tuple;

/**
 * A fire of {@code L_fork}, the {@code if} of a bool nobody could answer.
 *
 * <p>It takes the operands of the fire and the symbol table. It records
 * the fork, then asks phino for each arm dataized and stays suspended
 * while phino evaluates it, so that the rows of an arm land between the
 * row that opens it and the one that closes it. It answers a marker in the
 * carrier both arms agree on; an arm carrying nothing takes the carrier of
 * the other, since the two answer the same object.</p>
 *
 * @since 0.76.0
 */
final class Forking implements Fire {

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
    Forking(final Operands operands, final Symbols symbols) {
        this.args = operands;
        this.table = symbols;
    }

    @Override
    public String answer() throws IOException, InterruptedException {
        final String sym = this.table.fresh(
            "object", Arrays.asList("fork", this.args.of("guard", ""))
        );
        final List<String> values = new ArrayList<>(2);
        for (final String arm : Arrays.asList("left", "right")) {
            this.table.record(sym, arm);
            final String value = this.args.of(arm, "");
            this.table.record(sym, arm, "answer", value);
            values.add(value);
        }
        this.table.record(sym, "end");
        final String carrier = values.stream()
            .map(this::carried)
            .filter(forma -> !forma.isEmpty() && !"object".equals(forma))
            .findFirst()
            .orElse("object");
        for (final String value : values) {
            if (value.startsWith("sym:")) {
                this.table.witnessed(value.substring(4), carrier);
            }
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
