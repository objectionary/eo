/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.cactoos.Text;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.w3c.dom.Node;

/**
 * The φ-expression that walks from Φ down into one fragment.
 *
 * <p>It takes a document, the locator of the fragment in it, the formas of
 * the build and the symbol table. It answers one line of φ-calculus for
 * phino to morph inside, and on the way it seeds the table with one
 * {@code void} row per argument, so that every argument enters the run as
 * a symbol rather than an unknown. A stylesheet tells the way down, so no
 * Java here reads the document.</p>
 *
 * @since 0.77.0
 */
final class Applied {

    /**
     * The document the fragment stands in.
     */
    private final Node doc;

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
    Applied(final Node xmir, final String place,
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
        final List<String> steps = this.steps();
        final List<String> segments = new ArrayList<>(steps.size());
        String prefix = "";
        for (int idx = steps.size() - 1; idx >= 0; --idx) {
            segments.add(0, this.segment(steps.get(idx).split("\t", -1), prefix));
            prefix = String.format("ρ.%s", prefix);
        }
        return String.join(".", segments);
    }

    private List<String> steps() throws IOException {
        final List<String> out = new ArrayList<>(0);
        for (final Text line : new Split(new TextOf(this.walked()), "\\R")) {
            final String row = new UncheckedText(line).asString();
            if (!row.isEmpty()) {
                out.add(row);
            }
        }
        if (out.isEmpty()) {
            throw new IllegalStateException(
                String.format("The document has no element at '%s'", this.locator)
            );
        }
        return out;
    }

    private String walked() throws IOException {
        return new XSLDocument(
            Applied.class.getResource("/org/eolang/lowering/applying.xsl"), "applying.xsl"
        ).with("locator", this.locator).applyTo(new XMLDocument(this.doc));
    }

    private String segment(final String[] cells, final String prefix) throws IOException {
        final Carrier carrier = new Carrier(cells[0]);
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
            out = this.filled(cells, prefix);
        }
        return out;
    }

    private String filled(final String[] cells, final String prefix) throws IOException {
        final List<String> fills = new ArrayList<>(0);
        for (final String name : cells[3].split(" ", -1)) {
            if (!name.isEmpty()) {
                fills.add(String.format("%s ↦ %s", name, this.marker(cells[0], name, prefix)));
            }
        }
        String name = cells[1];
        if ("1".equals(cells[2])) {
            name = String.format("Φ.%s", name);
        }
        final String out;
        if (fills.isEmpty()) {
            out = name;
        } else {
            out = String.format("%s( %s )", name, String.join(", ", fills));
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
