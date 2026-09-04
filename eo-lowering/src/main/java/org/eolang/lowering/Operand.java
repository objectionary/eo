/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * One term of an evaluation record, read by its shape alone.
 *
 * <p>The text is what phino printed into a record: an argument binding,
 * or the result of a fired atom. No phi syntax is parsed here; instead
 * the whole text is matched against the few shapes whose meaning is
 * certain — a bytes application wrapping a {@code Δ} datum or a
 * {@code Sym_*} marker, a number or a string application wrapping such a
 * bytes application, a bare {@code Φ.true} or {@code Φ.false} — and
 * everything else is refused as unanchored. An unreduced application
 * renders with more around it than these shapes allow, so the refusal is
 * exactly the sign that the term is not yet a value; the reduction loop
 * then waits for a later iteration to reduce it. The α-named and the
 * resolved binding names are both accepted, since phino resolves them
 * lazily and a record may show either.</p>
 *
 * @since 0.76.0
 */
public final class Operand {

    /**
     * What the datum of a carrier looks like.
     */
    private static final String DATUM =
        "Δ ⤍ (--|[0-9A-F]{2}-|[0-9A-F]{2}(?:-[0-9A-F]{2})+)";

    /**
     * What the marker of a symbolic carrier looks like.
     */
    private static final String MARKER = "λ ⤍ Sym_(\\w+)";

    /**
     * The shapes of a value, each mapped to the forma it names.
     */
    private static final Map<Pattern, String> SHAPES = Operand.shapes();

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
        if ("Φ.true".equals(this.text)) {
            out = "bool:01-";
        } else if ("Φ.false".equals(this.text)) {
            out = "bool:00-";
        } else {
            for (final Map.Entry<Pattern, String> shape : Operand.SHAPES.entrySet()) {
                final Matcher found = shape.getKey().matcher(this.text);
                if (found.matches()) {
                    out = String.format("%s:%s", shape.getValue(), found.group(1));
                    break;
                }
            }
        }
        return out;
    }

    private static Map<Pattern, String> shapes() {
        final Map<Pattern, String> out = new LinkedHashMap<>(6);
        out.put(Pattern.compile(Operand.bytes(Operand.DATUM)), "bytes");
        out.put(Pattern.compile(Operand.bytes(Operand.MARKER)), "sym");
        for (final String carrier : new String[] {"number", "string"}) {
            out.put(Pattern.compile(Operand.carried(carrier, Operand.DATUM)), carrier);
            out.put(Pattern.compile(Operand.carried(carrier, Operand.MARKER)), "sym");
        }
        return out;
    }

    private static String carried(final String forma, final String payload) {
        return String.format(
            "Φ\\.%s\\( (?:as-bytes|α0) ↦ %s \\)", forma, Operand.bytes(payload)
        );
    }

    private static String bytes(final String payload) {
        return String.format("Φ\\.bytes\\( (?:data|α0) ↦ ⟦ %s, ρ ↦ ∅ ⟧ \\)", payload);
    }
}
