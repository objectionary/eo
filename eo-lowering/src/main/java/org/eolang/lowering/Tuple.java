/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.Arrays;

/**
 * A tuple phino can go on with, spelled from symbols.
 *
 * <p>It takes a symbol standing for a tuple and the symbol table. It
 * answers {@code Φ.tuple} applied to a length, a head and a tail, each a
 * fresh symbol recorded against the tuple, so that a body reading
 * {@code items.length} meets a number and the table still says where that
 * number came from. A tuple is not data, so no marker in a carrier could
 * stand for it.</p>
 *
 * @since 0.76.0
 */
public final class Tuple {

    /**
     * The symbol of the tuple.
     */
    private final String symbol;

    /**
     * The table.
     */
    private final Symbols table;

    /**
     * Ctor.
     *
     * @param sym The symbol of the tuple
     * @param symbols The table
     */
    public Tuple(final String sym, final Symbols symbols) {
        this.symbol = sym;
        this.table = symbols;
    }

    /**
     * The φ-expression.
     *
     * @return The text
     * @throws IOException If the table cannot be written
     */
    public String phi() throws IOException {
        return String.format(
            "Φ.tuple( length ↦ %s, head ↦ %s, tail ↦ %s )",
            new Marker(this.part("length", "number"), "number").phi(),
            new Marker(this.part("head", "object"), "object").phi(),
            new Marker(this.part("tail", "tuple"), "object").phi()
        );
    }

    private String part(final String name, final String carrier) throws IOException {
        return String.format(
            "sym:%s",
            this.table.minted(
                carrier,
                Arrays.asList("attr", String.format("sym:%s", this.symbol), name)
            )
        );
    }
}
