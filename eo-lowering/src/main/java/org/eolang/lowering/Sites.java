/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The places in a fragment where phino left a marker.
 *
 * <p>It takes the fragment and the symbol table. It answers one entry per
 * marker: the element to replace — the formation holding the marker
 * together with the carrier wrapped around it — and the symbol that marker
 * stands for. A marker that is the whole {@code φ} of the fragment comes
 * first, since lowering it swallows the rest.</p>
 *
 * @since 0.77.0
 */
final class Sites {

    /**
     * The symbols.
     */
    private static final Pattern SYMBOL = Pattern.compile("S\\d+");

    /**
     * The fragment.
     */
    private final Element fragment;

    /**
     * The table of the run.
     */
    private final Table table;

    /**
     * Ctor.
     *
     * @param formation The fragment
     * @param symbols The table of the run
     */
    Sites(final Element formation, final Table symbols) {
        this.fragment = formation;
        this.table = symbols;
    }

    /**
     * All sites, in the order they must be turned.
     *
     * @return The symbol of every site, by the site
     */
    Map<Element, String> all() {
        final Map<Element, String> out = new LinkedHashMap<>(0);
        final Map<Element, String> rest = new LinkedHashMap<>(0);
        for (final Element lambda : Sites.lambdas(this.fragment, new ArrayList<>(0))) {
            Element site = Sites.carried((Element) lambda.getParentNode());
            String sym = lambda.getTextContent();
            final String owner = this.table.receiver(sym);
            final Element parent = (Element) site.getParentNode();
            if ("Φ.tuple".equals(parent.getAttribute("base")) && !owner.isEmpty()) {
                site = parent;
                sym = owner;
            }
            if (site.getParentNode().equals(this.fragment)
                && "φ".equals(site.getAttribute("name"))) {
                out.put(site, sym);
            } else {
                rest.put(site, sym);
            }
        }
        out.putAll(rest);
        return out;
    }

    private static Element carried(final Element marker) {
        Element site = marker;
        Element parent = (Element) site.getParentNode();
        if (Sites.wraps(parent, "Φ.bytes")) {
            site = parent;
            parent = (Element) site.getParentNode();
            if (Sites.wraps(parent, "Φ.number") || Sites.wraps(parent, "Φ.string")) {
                site = parent;
            }
        } else if ("guard".equals(site.getAttribute("name"))
            && Sites.wraps(parent.getParentNode(), "Φ.bool")) {
            site = (Element) parent.getParentNode();
        }
        return site;
    }

    private static boolean wraps(final Node node, final String base) {
        return node.getNodeType() == Node.ELEMENT_NODE
            && base.equals(((Element) node).getAttribute("base"))
            && new Kids(node).all().size() == 1;
    }

    private static List<Element> lambdas(final Element node, final List<Element> out) {
        for (final Element kid : new Kids(node)) {
            if ("λ".equals(kid.getAttribute("name"))
                && Sites.SYMBOL.matcher(kid.getTextContent()).matches()) {
                out.add(kid);
            } else {
                Sites.lambdas(kid, out);
            }
        }
        return out;
    }
}
