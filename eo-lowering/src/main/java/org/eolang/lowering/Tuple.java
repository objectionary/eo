/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.Arrays;

/**
 * A symbolic tuple, spelled as a tuple whose parts are symbols of their own.
 *
 * <p>A tuple is not data, so no marker inside a carrier can stand for it.
 * Instead it stands as {@code Φ.tuple( length ↦ …, head ↦ …, tail ↦ … )}
 * with one fresh symbol per part, each recorded as an {@code attr} row of
 * the tuple's symbol, so that a body reading {@code items.length} meets a
 * number marker and a body reading {@code items.head} meets a bare one,
 * and the table says where each came from.</p>
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

    /**
     * The symbol of one part.
     *
     * @param name The name of the attribute
     * @param carrier The carrier of the part
     * @return The key of the part
     * @throws IOException If the table cannot be written
     */
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
