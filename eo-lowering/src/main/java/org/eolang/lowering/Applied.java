/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The φ-expression that walks from Φ down into one fragment.
 *
 * <p>It takes a document, the locator of the fragment in it, the formas of
 * the build and the symbol table. It answers one line of φ-calculus for
 * phino to morph inside, and on the way it seeds the table with one
 * {@code void} row per argument, so that every argument enters the run as
 * a symbol rather than an unknown.</p>
 *
 * @since 0.77.0
 */
final class Applied {

    /**
     * The document the fragment stands in.
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
     * @param xmir The document the fragment stands in
     * @param place The locator of the fragment
     * @param tables The formas of the build
     * @param symbols The table of symbols
     */
    Applied(final Document xmir, final String place,
        final Formas tables, final Symbols symbols) {
        this.doc = xmir;
        this.locator = place;
        this.formas = tables;
        this.table = symbols;
    }

    /**
     * The φ-expression.
     *
     * @return The text
     * @throws IOException If the table cannot be written
     */
    String phi() throws IOException {
        final List<String> segments = new ArrayList<>(0);
        Node cursor = new Located(this.doc.getDocumentElement(), this.locator).element();
        String prefix = "";
        while (cursor != null && cursor.getNodeType() == Node.ELEMENT_NODE
            && "o".equals(cursor.getNodeName())) {
            final Element formation = (Element) cursor;
            if (!formation.hasAttribute("base")) {
                segments.add(0, this.segment(formation, prefix));
                prefix = String.format("ρ.%s", prefix);
            }
            cursor = cursor.getParentNode();
        }
        return String.join(".", segments);
    }

    private String segment(final Element formation, final String prefix) throws IOException {
        final String place = formation.getAttribute("loc");
        final Carrier carrier = new Carrier(place);
        final String out;
        if (carrier.data() && !"tuple".equals(carrier.forma())) {
            out = new Marker(
                String.format(
                    "sym:%s",
                    this.table.minted(
                        carrier.forma(),
                        Arrays.asList(
                            "void", prefix.substring(0, Math.max(0, prefix.length() - 1))
                        )
                    )
                ),
                carrier.forma()
            ).phi();
        } else {
            final List<String> fills = new ArrayList<>(0);
            for (final Element kid : new Kids(formation)) {
                final String name = kid.getAttribute("name");
                if ("∅".equals(kid.getAttribute("base")) && !"ρ".equals(name)) {
                    fills.add(
                        String.format(
                            "%s ↦ %s", name, this.marker(place, name, prefix)
                        )
                    );
                }
            }
            String name = formation.getAttribute("name");
            if (!"o".equals(formation.getParentNode().getNodeName())) {
                name = String.format("Φ.%s", name);
            }
            if (fills.isEmpty()) {
                out = name;
            } else {
                out = String.format("%s( %s )", name, String.join(", ", fills));
            }
        }
        return out;
    }

    private String marker(final String place, final String name, final String prefix)
        throws IOException {
        String forma = this.formas.known(String.format("%s.%s", place, name));
        if (forma.isEmpty()) {
            forma = "object";
        }
        final String sym = this.table.minted(
            forma, Arrays.asList("void", String.format("%s%s", prefix, name))
        );
        final String out;
        if ("tuple".equals(forma)) {
            out = new Tuple(sym, this.table).phi();
        } else {
            out = new Marker(String.format("sym:%s", sym), forma).phi();
        }
        return out;
    }
}
