/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A φ-expression read as an operand of the table.
 *
 * <p>An operand is a symbol, {@code sym:S4}, when the text is a marker
 * {@code ⟦ λ ⤍ S4 ⟧} inside any carrier, or a literal, {@code number:HEX}
 * or {@code bytes:HEX}, when it is a Δ formation inside a carrier. The
 * prefix of a literal says how it was learned: the carrier when the text
 * spelled one, {@code bytes} when it was a bare Δ formation. Anything else
 * reads as an empty key, since an expression that is neither a marker nor
 * data is not yet an operand and has to be asked for.</p>
 *
 * <p>Both ends of the shapes are anchored on purpose: a marker is also the
 * prefix of an application of it, and a prefix match would mint a row for
 * the wrong operand without failing.</p>
 *
 * @since 0.76.0
 */
public final class Operand {

    /**
     * A marker or a datum inside a formation.
     */
    private static final String PAYLOAD = "(?:λ ⤍ (S\\d+)|Δ ⤍ (--|[0-9A-F][0-9A-F-]*))";

    /**
     * The payload and the closing bracket of its formation.
     */
    private static final String LOAD = String.format("%s(?:, ρ ↦ ∅)? ⟧", Operand.PAYLOAD);

    /**
     * Bytes carrying a payload.
     */
    private static final Pattern AS_BYTES = Pattern.compile(
        String.format("^Φ\\.bytes\\( φ ↦ ⟦ %s \\)$", Operand.LOAD)
    );

    /**
     * A typed carrier holding bytes with a payload.
     */
    private static final Pattern AS_TYPED = Pattern.compile(
        String.format(
            "^Φ\\.(number|string|bytes|bool)\\( (?:as-bytes|φ) ↦ Φ\\.bytes\\( φ ↦ ⟦ %s \\) \\)$",
            Operand.LOAD
        )
    );

    /**
     * A bool whose {@code if} is a fork on a symbol.
     */
    private static final Pattern GUARD = Pattern.compile(
        "^(?:Φ\\.bool\\( if ↦ )?⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ (S\\d+) ⟧, λ ⤍ L_fork ⟧"
    );

    /**
     * The shapes an answer of phino takes.
     */
    private static final List<Pattern> SHAPES = Arrays.asList(
        Pattern.compile(String.format("^⟦ %s", Operand.PAYLOAD)),
        Pattern.compile(String.format("^Φ\\.bytes\\( φ ↦ ⟦ %s", Operand.PAYLOAD)),
        Pattern.compile("^()(--|[0-9A-F][0-9A-F-]*)$")
    );

    /**
     * The text.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param phi The φ-expression
     */
    public Operand(final String phi) {
        this.text = phi;
    }

    /**
     * The key of this operand.
     *
     * @return The key, or an empty string when the text is not an operand yet
     */
    public String key() {
        final String value = this.text.replaceAll("\\s+", " ").trim();
        String out = Operand.wrapped(value);
        if (out.isEmpty()) {
            final Matcher guard = Operand.GUARD.matcher(value);
            if (guard.find()) {
                out = String.format("sym:%s", guard.group(1));
            }
        }
        if (out.isEmpty() && value.startsWith("⟦")) {
            final Map<String, String> held = new Bindings(value).all();
            if (held.containsKey("if")) {
                out = new Operand(held.get("if")).key();
            } else {
                out = Operand.wrapped(held.getOrDefault("φ", held.getOrDefault("as-bytes", "")));
            }
        }
        if (out.isEmpty()) {
            out = Operand.witnessed(value);
        }
        return out;
    }

    /**
     * Read a payload inside a carrier.
     *
     * @param value The text
     * @return The key, or an empty string
     */
    private static String wrapped(final String value) {
        final Matcher typed = Operand.AS_TYPED.matcher(value);
        final String out;
        if (typed.matches()) {
            out = Operand.keyed(typed.group(1), typed.group(2), typed.group(3));
        } else {
            final Matcher bare = Operand.AS_BYTES.matcher(value);
            if (bare.matches()) {
                out = Operand.keyed("bytes", bare.group(1), bare.group(2));
            } else {
                out = "";
            }
        }
        return out;
    }

    /**
     * Read a payload the way phino answers a question.
     *
     * @param value The text
     * @return The key, or an empty string
     */
    private static String witnessed(final String value) {
        String out = "";
        for (final Pattern shape : Operand.SHAPES) {
            final Matcher seen = shape.matcher(value);
            if (seen.find()) {
                out = Operand.keyed("bytes", seen.group(1), seen.group(2));
                break;
            }
        }
        return out;
    }

    /**
     * Make a key of a payload.
     *
     * @param carrier The carrier the literal would take
     * @param symbol The symbol, or null
     * @param hex The bytes, or null
     * @return The key
     */
    private static String keyed(final String carrier, final String symbol, final String hex) {
        final String out;
        if (symbol == null || symbol.isEmpty()) {
            out = String.format("%s:%s", carrier, hex);
        } else {
            out = String.format("sym:%s", symbol);
        }
        return out;
    }
}
