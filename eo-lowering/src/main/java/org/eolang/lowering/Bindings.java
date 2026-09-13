/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.HashMap;
import java.util.Map;

/**
 * The bindings of the formation phino hands over under {@code 𝑏}.
 *
 * <p>The text is a formation, {@code ⟦ x ↦ …, y ↦ … ⟧}, split at depth
 * zero on commas and arrows; a binding whose value is an application or a
 * nested formation is kept as one piece, brackets and all. This is the
 * whole of the φ syntax the engine parses.</p>
 *
 * @since 0.76.0
 */
public final class Bindings {

    /**
     * The text of the formation.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param body The text of the formation
     */
    public Bindings(final String body) {
        this.text = body;
    }

    /**
     * The value bound to a name.
     *
     * @param name The name
     * @return The value as written, or an empty string when unbound
     */
    public String of(final String name) {
        return this.all().getOrDefault(name, "");
    }

    /**
     * All bindings.
     *
     * @return The map from names to values as written
     */
    public Map<String, String> all() {
        final Map<String, String> out = new HashMap<>(0);
        final String body = this.text.replaceAll("\\s+", " ").trim();
        final StringBuilder piece = new StringBuilder(0);
        int depth = 0;
        for (final int glyph : body.substring(Math.min(1, body.length())).codePoints().toArray()) {
            if (glyph == '⟦' || glyph == '(') {
                depth += 1;
            } else if (glyph == '⟧' || glyph == ')') {
                if (depth == 0) {
                    break;
                }
                depth -= 1;
            }
            if (depth == 0 && glyph == ',') {
                Bindings.bound(out, piece.toString());
                piece.setLength(0);
            } else {
                piece.appendCodePoint(glyph);
            }
        }
        Bindings.bound(out, piece.toString());
        return out;
    }

    /**
     * Put one binding into the map.
     *
     * @param out The map
     * @param piece The text of the binding
     */
    private static void bound(final Map<String, String> out, final String piece) {
        final int arrow = piece.indexOf('↦');
        if (arrow < 0) {
            if (!piece.trim().isEmpty()) {
                out.put(piece.trim(), "");
            }
        } else {
            out.put(piece.substring(0, arrow).trim(), piece.substring(arrow + 1).trim());
        }
    }
}
