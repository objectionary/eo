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
 * <p>A number that is not a number comes back in one shape only. IEEE 754
 * leaves the payload of a NaN open, and phino keeps whatever bits its
 * host produced: an x86 division of zero by zero sets the sign bit and
 * yields {@code FF-F8-00-00-00-00-00-00}, while an ARM one does not.
 * EO knows a single {@code nan}, the object of the bytes
 * {@code 7F-F8-00-00-00-00-00-00}, and the JVM runtime turns every NaN
 * an atom computes into that very object, so a folded literal must carry
 * the same bytes, or {@code (0.div 0).as-bytes} would stop being equal to
 * {@code nan.as-bytes} once the fragment is folded at build time.</p>
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
     *
     * <p>A NaN number is canonicalized to the bytes of {@code nan},
     * every other value comes back exactly as phino printed it.</p>
     *
     * @return Dash-joined hex pairs
     */
    public String bytes() {
        final String out;
        if (this.numeric() && Datum.nan(this.hex)) {
            out = "7F-F8-00-00-00-00-00-00";
        } else {
            out = this.hex;
        }
        return out;
    }

    /**
     * The forma of the value.
     * @return One of {@code number}, {@code bool}, {@code bytes}
     */
    public String forma() {
        final String out;
        if ("Φ.true".equals(this.term) || "Φ.false".equals(this.term)) {
            out = "bool";
        } else if (this.numeric()) {
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

    private boolean numeric() {
        return this.term.startsWith("Φ.number");
    }

    private static boolean nan(final String hex) {
        final String digits = hex.replace("-", "");
        return digits.length() == 16
            && Double.isNaN(
                Double.longBitsToDouble(Long.parseUnsignedLong(digits, 16))
            );
    }
}
