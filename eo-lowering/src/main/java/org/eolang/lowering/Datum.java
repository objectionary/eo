/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * The value of one dataization: its bytes and the carrier they belong in.
 *
 * <p>The bytes are what phino printed. The term is what the last atom
 * evaluation returned, taken from the protocol phino writes when asked
 * with {@code --evaluations}: atoms answer with typed terms, such as a
 * {@code Φ.number} application wrapping the bytes for arithmetic and a
 * bare {@code Φ.true} for a comparison, and the outermost atom of a fragment
 * fires last, so the final record names the carrier of the whole value.
 * No table of atom names lives on this side: whatever the term starts
 * with decides the forma. A dataization that fired no atom leaves the
 * term empty, and such a value has no known forma — asking for it is
 * refused, since a guessed carrier would miscompile the program.</p>
 *
 * @since 0.76.0
 */
public final class Datum {

    /**
     * The dataized bytes, as dash-joined hex pairs.
     */
    private final String hex;

    /**
     * The term the last atom evaluation returned, possibly empty.
     */
    private final String term;

    /**
     * Ctor.
     * @param bytes The dataized bytes, as dash-joined hex pairs
     * @param answer The term the last atom evaluation returned, or empty
     */
    public Datum(final String bytes, final String answer) {
        this.hex = bytes;
        this.term = answer;
    }

    /**
     * The bytes.
     * @return Dash-joined hex pairs
     */
    public String bytes() {
        return this.hex;
    }

    /**
     * The forma of the value.
     * @return One of {@code number}, {@code bool}, {@code bytes}
     */
    public String forma() {
        final String out;
        if ("Φ.true".equals(this.term) || "Φ.false".equals(this.term)) {
            out = "bool";
        } else if (this.term.startsWith("Φ.number")) {
            out = "number";
        } else if (this.term.startsWith("Φ.bytes")) {
            out = "bytes";
        } else {
            throw new IllegalStateException(
                String.format(
                    "No literal carrier matches the term '%s' of the last atom evaluation",
                    this.term
                )
            );
        }
        return out;
    }
}
