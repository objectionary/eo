/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * A fire of a box, the λ planted on a formation that declares arguments.
 *
 * <p>Every entry into the body of another fragment is a boundary, since
 * that fragment is lowered by a run of its own: the fire records a
 * {@code box} row naming the formation and the operands bound at this
 * entry, and answers a marker in the carrier the formation is known to
 * answer. The body is never evaluated here. When the body reaches for its
 * ρ, the receiver is asked for as written and read as a value; a receiver
 * that turns out to be the formation the box was written in is the
 * lexical parent, which Java reaches by name and no row has to carry.</p>
 *
 * @since 0.76.0
 */
public final class Boxing implements Fire {

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
    public Boxing(final Box planted, final Operands operands, final Symbols symbols) {
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
            final String key = this.args.receiver(this.box.lambda());
            if (!key.isEmpty()) {
                cells.add(String.format("ρ=%s", Boxing.typed(key, this.box.parent())));
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

    /**
     * Give an untyped datum the forma it is known to carry.
     *
     * @param key The key
     * @param forma The forma, or {@code object} when unknown
     * @return The key, typed when it can be
     */
    private static String typed(final String key, final String forma) {
        final String out;
        if (key.startsWith("bytes:") && !"object".equals(forma)) {
            out = String.format("%s:%s", forma, key.substring(6));
        } else {
            out = key;
        }
        return out;
    }
}
