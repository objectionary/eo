/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.Arrays;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The voids of one fragment, and of every formation enclosing it, filled
 * with markers of fresh symbols, each seeded into the table as a
 * {@code void} row naming the path of the void from the fragment.
 *
 * <p>The enclosing formations are entered on the way in, since the run
 * dispatches into the fragment through them, and a void of theirs left
 * open would read as ⊥ from the body. Their paths carry a {@code ρ.} per
 * level. The receiver void {@code ρ} itself is left alone, since the
 * dispatch binds it to the real parent, which is how the body reaches
 * its neighbours. When the parent is a data forma, its one void holds
 * the payload of a marker rather than a marker of its own, so that the
 * parent reads as a value of that forma, seeded as the void {@code ρ}
 * the body reaches through.</p>
 *
 * @since 0.77.0
 */
public final class Symbolized {

    /**
     * The document to plant into.
     */
    private final Document doc;

    /**
     * The locator of the fragment.
     */
    private final String locator;

    /**
     * The formas of the build.
     */
    private final Formas formas;

    /**
     * The table of symbols.
     */
    private final Symbols table;

    /**
     * Ctor.
     *
     * @param xmir The document to plant into
     * @param place The locator of the fragment
     * @param tables The formas of the build
     * @param symbols The table of symbols
     */
    public Symbolized(final Document xmir, final String place,
        final Formas tables, final Symbols symbols) {
        this.doc = xmir;
        this.locator = place;
        this.formas = tables;
        this.table = symbols;
    }

    /**
     * Plant the markers.
     *
     * @throws IOException If the table cannot be written
     */
    public void plant() throws IOException {
        Node cursor = new Located(this.doc.getDocumentElement(), this.locator).element();
        String prefix = "";
        while (cursor != null && cursor.getNodeType() == Node.ELEMENT_NODE
            && "o".equals(cursor.getNodeName())) {
            final Element formation = (Element) cursor;
            if (!formation.hasAttribute("base")) {
                this.filled(formation, prefix);
                prefix = String.format("ρ.%s", prefix);
            }
            cursor = cursor.getParentNode();
        }
    }

    private void filled(final Element formation, final String prefix) throws IOException {
        final String place = formation.getAttribute("loc");
        final String carrier = this.formas.at(place);
        for (final Element kid : new Kids(formation)) {
            final String name = kid.getAttribute("name");
            if ("∅".equals(kid.getAttribute("base")) && !"ρ".equals(name)) {
                if (this.formas.data(place) && !"tuple".equals(carrier)) {
                    new Slot(
                        this.table.minted(
                            carrier,
                            Arrays.asList(
                                "void", prefix.substring(0, Math.max(0, prefix.length() - 1))
                            )
                        ),
                        carrier, this.table
                    ).under(kid);
                } else {
                    String forma = this.formas.known(String.format("%s.%s", place, name));
                    if (forma.isEmpty()) {
                        forma = "object";
                    }
                    new Slot(
                        this.table.minted(
                            forma, Arrays.asList("void", String.format("%s%s", prefix, name))
                        ),
                        forma, this.table
                    ).into(kid);
                }
            }
        }
    }
}
