/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * One term of an evaluation record, read by its shape alone.
 *
 * <p>The text is what phino printed into a record: an argument binding,
 * or the result of a fired atom. No phi syntax is parsed here; instead
 * the whole text is matched against the few shapes whose meaning is
 * certain — a literal carrier application wrapping a {@code Δ} datum, a
 * symbolic carrier application wrapping a {@code Sym_*} marker, a bare
 * {@code Φ.true} or {@code Φ.false} — and everything else is refused as
 * unanchored. An unreduced application renders with more around it than
 * these shapes allow, so the refusal is exactly the sign that the term
 * is not yet a value; the reduction loop then waits for a later
 * iteration to reduce it. The α-named and the resolved binding names are
 * both accepted, since phino resolves them lazily and a record may show
 * either.</p>
 *
 * @since 0.76.0
 */
public final class Operand {

    /**
     * A number carrier holding a datum.
     */
    private static final Pattern NUMBER = Pattern.compile(
        "Φ\\.number\\( (?:as-bytes|α0) ↦ Φ\\.bytes\\( (?:data|α0) ↦ ⟦ Δ ⤍ (--|[0-9A-F]{2}-|[0-9A-F]{2}(?:-[0-9A-F]{2})+), ρ ↦ ∅ ⟧ \\) \\)"
    );

    /**
     * A number carrier holding a marker.
     */
    private static final Pattern SYMBOL = Pattern.compile(
        "Φ\\.number\\( (?:as-bytes|α0) ↦ Φ\\.bytes\\( (?:data|α0) ↦ ⟦ λ ⤍ Sym_(\\w+), ρ ↦ ∅ ⟧ \\) \\)"
    );

    /**
     * A bytes carrier holding a datum.
     */
    private static final Pattern BYTES = Pattern.compile(
        "Φ\\.bytes\\( (?:data|α0) ↦ ⟦ Δ ⤍ (--|[0-9A-F]{2}-|[0-9A-F]{2}(?:-[0-9A-F]{2})+), ρ ↦ ∅ ⟧ \\)"
    );

    /**
     * A bytes carrier holding a marker.
     */
    private static final Pattern MARKER = Pattern.compile(
        "Φ\\.bytes\\( (?:data|α0) ↦ ⟦ λ ⤍ Sym_(\\w+), ρ ↦ ∅ ⟧ \\)"
    );

    /**
     * The text of the term, as one record field holds it.
     */
    private final String text;

    /**
     * Ctor.
     * @param term The text of the term, as one record field holds it
     */
    public Operand(final String term) {
        this.text = term;
    }

    /**
     * Whether the shape of the term names a value.
     * @return True if {@link #key()} would answer
     */
    public boolean anchored() {
        return !this.guessed().isEmpty();
    }

    /**
     * The identity of the value.
     * @return A key such as {@code sym:s1}, {@code number:40-14-...} or {@code bool:01-}
     */
    public String key() {
        final String out = this.guessed();
        if (out.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "The term '%s' is not anchored to any value shape",
                    this.text
                )
            );
        }
        return out;
    }

    private String guessed() {
        String out = "";
        final Matcher number = Operand.NUMBER.matcher(this.text);
        final Matcher symbol = Operand.SYMBOL.matcher(this.text);
        final Matcher bytes = Operand.BYTES.matcher(this.text);
        final Matcher marker = Operand.MARKER.matcher(this.text);
        if ("Φ.true".equals(this.text)) {
            out = "bool:01-";
        } else if ("Φ.false".equals(this.text)) {
            out = "bool:00-";
        } else if (number.matches()) {
            out = String.format("number:%s", number.group(1));
        } else if (symbol.matches()) {
            out = String.format("sym:%s", symbol.group(1));
        } else if (bytes.matches()) {
            out = String.format("bytes:%s", bytes.group(1));
        } else if (marker.matches()) {
            out = String.format("sym:%s", marker.group(1));
        }
        return out;
    }
}
