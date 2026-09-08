/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The symbol table one morph of a fragment leaves, as a protocol.
 *
 * <p>The atom writes one row per value the fragment computes: the symbol
 * it mints, the forma that symbol carries, the λ of the atom that minted
 * it, and the operands it read, each of them either {@code sym:} and the
 * symbol of an earlier row or {@code hex:} and the bytes of a literal. A
 * row minted by {@code void} stands for a void of the fragment instead,
 * and such rows come in the order the voids are declared; every other row
 * is one {@link Application} labelled after its symbol, whose literal
 * operands take the formas the {@link Op} row of its atom declares, since
 * a row names the forma of its own value alone. The last row is the
 * answer, since the outermost atom fires last.</p>
 *
 * @since 0.76.0
 * @todo #8548:30min Read the rows of a choice and of a repeat, which are
 *  the {@link Fork} and the {@link Repeat} this module renders already,
 *  and write the registry naming the λ functions together with the atom
 *  that mints these rows, since until they land nothing calls this class.
 */
public final class Table {

    /**
     * The rows, tab-separated, in the order they were recorded.
     */
    private final List<String> rows;

    /**
     * Ctor.
     *
     * @param records The rows, tab-separated, in the order recorded
     */
    public Table(final List<String> records) {
        this.rows = records;
    }

    /**
     * The protocol the table spells.
     *
     * @return The steps in their order, the key of the answer, and the
     *  forma that answer carries
     */
    public Protocol protocol() {
        final Map<String, String> keys = new HashMap<>(0);
        final List<Step> steps = new ArrayList<>(0);
        String answer = "";
        String forma = "";
        int voids = 0;
        for (final String row : this.rows) {
            final String[] cells = row.split("\t");
            if ("void".equals(cells[2])) {
                answer = String.format("sym:v%d", voids);
                voids += 1;
            } else {
                final String label = cells[0].toLowerCase(Locale.ENGLISH);
                answer = String.format("sym:%s", label);
                steps.add(new Application(label, cells[2], Table.operands(cells, keys)));
            }
            keys.put(cells[0], answer);
            forma = cells[1];
        }
        return new Protocol(steps, answer, forma);
    }

    private static List<String> operands(final String[] cells, final Map<String, String> keys) {
        final Op operation = new Op(cells[2]);
        final List<String> formas = new ArrayList<>(cells.length);
        formas.add(operation.carrier());
        formas.addAll(operation.formas());
        final List<String> out = new ArrayList<>(cells.length - 3);
        for (int idx = 3; idx < cells.length; ++idx) {
            final String[] parts = cells[idx].split(":", 2);
            if ("sym".equals(parts[0])) {
                if (!keys.containsKey(parts[1])) {
                    throw new IllegalStateException(
                        String.format(
                            "The table reads the symbol '%s' before any row mints it", parts[1]
                        )
                    );
                }
                out.add(keys.get(parts[1]));
            } else {
                out.add(String.format("%s:%s", formas.get(idx - 3), parts[1]));
            }
        }
        return out;
    }
}
