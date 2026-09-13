/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.Arrays;
import org.w3c.dom.Element;

/**
 * A void of an XMIR formation filled with the marker of a symbol, in the
 * carrier the void is witnessed to hold, the XMIR twin of {@link Marker}
 * and {@link Tuple}; or, when the void is the one of a data forma, with
 * the payload of that marker, so that the forma itself reads as it.
 *
 * @since 0.77.0
 */
final class Slot {

    /**
     * The symbol.
     */
    private final String symbol;

    /**
     * The carrier of the void.
     */
    private final String carrier;

    /**
     * The table of symbols, for the parts of a tuple.
     */
    private final Symbols table;

    /**
     * Ctor.
     *
     * @param sym The symbol
     * @param forma The carrier of the void
     * @param symbols The table of symbols
     */
    Slot(final String sym, final String forma, final Symbols symbols) {
        this.symbol = sym;
        this.carrier = forma;
        this.table = symbols;
    }

    /**
     * Fill the void.
     *
     * @param hole The void element, which loses its {@code ∅}
     * @throws IOException If the table cannot be written
     */
    void into(final Element hole) throws IOException {
        hole.removeAttribute("base");
        if ("number".equals(this.carrier) || "string".equals(this.carrier)) {
            hole.setAttribute("base", String.format("Φ.%s", this.carrier));
            hole.appendChild(Slot.wrapped(hole, "φ", "Φ.bytes", this.symbol));
        } else if ("bytes".equals(this.carrier)) {
            hole.setAttribute("base", "Φ.bytes");
            hole.appendChild(Slot.marker(hole, "φ", this.symbol));
        } else if ("bool".equals(this.carrier)) {
            hole.setAttribute("base", "Φ.bool");
            hole.appendChild(this.forked(hole));
        } else if ("tuple".equals(this.carrier)) {
            hole.setAttribute("base", "Φ.tuple");
            hole.appendChild(Slot.wrapped(hole, "length", "Φ.number", this.part("length", "number")));
            hole.appendChild(Slot.marker(hole, "head", this.part("head", "object")));
            hole.appendChild(Slot.marker(hole, "tail", this.part("tail", "tuple")));
        } else {
            hole.appendChild(Slot.lambda(hole, this.symbol));
        }
    }

    private String part(final String name, final String forma) throws IOException {
        return this.table.minted(
            forma, Arrays.asList("attr", String.format("sym:%s", this.symbol), name)
        );
    }

    void under(final Element hole) {
        hole.removeAttribute("base");
        if ("number".equals(this.carrier) || "string".equals(this.carrier)) {
            hole.setAttribute("base", "Φ.bytes");
            hole.appendChild(Slot.marker(hole, "φ", this.symbol));
        } else if ("bytes".equals(this.carrier)) {
            hole.appendChild(Slot.lambda(hole, this.symbol));
        } else if ("bool".equals(this.carrier)) {
            this.armed(hole);
        } else {
            throw new IllegalStateException(
                String.format(
                    "A %s has no payload to plant under the forma it stands in", this.carrier
                )
            );
        }
    }

    private Element forked(final Element hole) {
        final Element out = hole.getOwnerDocument().createElement("o");
        out.setAttribute("as", "if");
        this.armed(out);
        return out;
    }

    private void armed(final Element fork) {
        for (final String arm : Arrays.asList("left", "right")) {
            final Element vain = fork.getOwnerDocument().createElement("o");
            vain.setAttribute("base", "∅");
            vain.setAttribute("name", arm);
            fork.appendChild(vain);
        }
        final Element guard = fork.getOwnerDocument().createElement("o");
        guard.setAttribute("name", "guard");
        guard.appendChild(Slot.lambda(fork, this.symbol));
        fork.appendChild(guard);
        fork.appendChild(Slot.lambda(fork, "L_fork"));
    }

    private static Element wrapped(final Element hole, final String slot,
        final String base, final String sym) {
        final Element out = hole.getOwnerDocument().createElement("o");
        out.setAttribute("as", slot);
        out.setAttribute("base", base);
        out.appendChild(Slot.marker(hole, "φ", sym));
        return out;
    }

    private static Element marker(final Element hole, final String slot, final String sym) {
        final Element out = hole.getOwnerDocument().createElement("o");
        out.setAttribute("as", slot);
        out.appendChild(Slot.lambda(hole, sym));
        return out;
    }

    private static Element lambda(final Element hole, final String name) {
        final Element out = hole.getOwnerDocument().createElement("o");
        out.setAttribute("name", "λ");
        out.setTextContent(name);
        return out;
    }
}
