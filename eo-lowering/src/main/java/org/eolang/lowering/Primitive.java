/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * A fire of a primitive operation, one of the rows of {@code ops.tsv}.
 *
 * <p>The engine does not compute the operation; it records that it
 * happened, as one row of the table, and answers a fresh symbol in the
 * carrier of the result, so that phino goes on as if it had the value.
 * The one exception is an operation on literals alone, which is folded
 * here and answered as data, so that {@code 1.neg}, which the library
 * spells {@code 1.times -1}, mints no row.</p>
 *
 * @since 0.76.0
 */
public final class Primitive implements Fire {

    /**
     * The operation.
     */
    private final Op operation;

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
     * @param lambda The operation
     * @param operands The operands of the fire
     * @param symbols The table
     */
    public Primitive(final Op lambda, final Operands operands, final Symbols symbols) {
        this.operation = lambda;
        this.args = operands;
        this.table = symbols;
    }

    @Override
    public String answer() throws IOException, InterruptedException {
        final List<String> keys = new ArrayList<>(1 + this.operation.args().size());
        keys.add(this.args.of("ρ", this.operation.carrier()));
        for (int idx = 0; idx < this.operation.args().size(); ++idx) {
            keys.add(
                this.args.of(this.operation.args().get(idx), this.operation.formas().get(idx))
            );
        }
        String out = "";
        if (keys.stream().noneMatch(key -> key.startsWith("sym:"))) {
            out = new Folded(this.operation, keys).phi();
        }
        if (out.isEmpty()) {
            final List<String> cells = new ArrayList<>(keys.size() + 1);
            cells.add(this.operation.lambda());
            cells.addAll(keys);
            out = new Marker(
                String.format("sym:%s", this.table.minted(this.operation.forma(), cells)),
                this.operation.forma()
            ).phi();
        }
        return out;
    }
}
