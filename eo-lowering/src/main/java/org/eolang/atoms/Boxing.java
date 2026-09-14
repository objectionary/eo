/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.eolang.lowering.Box;
import org.eolang.lowering.Marker;
import org.eolang.lowering.Symbols;
import org.eolang.lowering.Tuple;

/**
 * A fire of a box: the λ of a formation that takes arguments.
 *
 * <p>It takes the box of that formation, the operands of the fire and the
 * symbol table. It records one {@code box} row naming the formation and
 * the arguments bound at this entry, and answers a marker in the carrier
 * the formation is known to answer. The body is never evaluated here,
 * since the fragment behind it is lowered by a run of its own.</p>
 *
 * @since 0.76.0
 * @todo #8548:30min Type the answer of a box nobody witnesses once
 *  eo-inference tells the forma of a formation whose φ forks between data
 *  of one forma: today such a box answers a bare marker, and a dispatch on
 *  that marker, the plus of classic-fibonacci over two recursive calls for
 *  one, stays stuck since the marker carries no plus, so the fragment stays
 *  as written. When the box knows its carrier, the classic-fibonacci pack
 *  must lower; adjust its expectations then.
 */
final class Boxing implements Fire {

    /**
     * The box.
     */
    private final Box box;

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
     * @param planted The box
     * @param operands The operands of the fire
     * @param symbols The table
     */
    Boxing(final Box planted, final Operands operands, final Symbols symbols) {
        this.box = planted;
        this.args = operands;
        this.table = symbols;
    }

    @Override
    public String answer() throws IOException, InterruptedException {
        final List<String> cells = new ArrayList<>(this.box.voids().size() + 3);
        cells.add("box");
        cells.add(this.box.locator());
        if (this.box.reaches()) {
            final String key = this.args.receiver(this.box);
            if (!key.isEmpty()) {
                cells.add(String.format("ρ=%s", key));
            }
        }
        for (final Map.Entry<String, String> entry : this.box.voids().entrySet()) {
            if (this.args.bound(entry.getKey())) {
                cells.add(
                    String.format(
                        "%s=%s", entry.getKey(), this.args.of(entry.getKey(), entry.getValue())
                    )
                );
            }
        }
        final String sym = this.table.minted(this.box.carrier(), cells);
        final String out;
        if ("tuple".equals(this.box.carrier())) {
            out = new Tuple(sym, this.table).phi();
        } else {
            out = new Marker(String.format("sym:%s", sym), this.box.carrier()).phi();
        }
        return out;
    }
}
