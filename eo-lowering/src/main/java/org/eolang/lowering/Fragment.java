/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * One application subtree, carved into a lowerable fragment.
 *
 * <p>The subtree is a copy of the site, so carving never touches the
 * document. A carve walks it once: a literal stays as it is, a dispatch
 * descends, and every symbolic reference — a bare one, or the receiver
 * prefix of a rolled base, which is exactly what the {@code .ρ} row of
 * the links table describes — becomes a void of the fragment, named
 * {@code v0}, {@code v1} and so on in the order of first appearance,
 * with its forma chased through {@link Formas}. Two references to the
 * same locator share one void. The carve refuses — and the site stays
 * as written — on a reference outside the carrier formas, a
 * formation where data is expected, a call of a sibling formation, or
 * a subtree of fewer than two operations, since a one-step computation
 * is a Java atom already and a synthetic clone of it would buy
 * nothing.</p>
 *
 * @since 0.76.0
 */
final class Fragment {

    /**
     * The bases that stand for data literals and stay in the body.
     */
    private static final Collection<String> LITERALS = new HashSet<>(
        Arrays.asList("Φ.number", "Φ.string", "Φ.bytes", "Φ.true", "Φ.false")
    );

    /**
     * The formas a symbolic carrier can stand for.
     */
    private static final Collection<String> CARRIERS = new HashSet<>(
        Arrays.asList("number", "string", "bytes", "bool")
    );

    /**
     * The copy of the site, carved in place.
     */
    private final Element body;

    /**
     * The void names, by the leaf base each one replaced.
     */
    private final Map<String, String> names;

    /**
     * The formas, by void name, in declaration order.
     */
    private final Map<String, String> formas;

    /**
     * The tables that decide the forma of a leaf.
     */
    private final Formas tables;

    /**
     * Ctor.
     * @param site The application to carve a copy of
     * @param decided The tables that decide the forma of a leaf
     */
    Fragment(final Element site, final Formas decided) {
        this(
            (Element) site.cloneNode(true),
            new LinkedHashMap<>(),
            new LinkedHashMap<>(),
            decided
        );
    }

    /**
     * Ctor.
     * @param copy The copy of the site, to carve in place
     * @param leaves The void names, by the leaf base each one replaced
     * @param voids The formas, by void name, in declaration order
     * @param decided The tables that decide the forma of a leaf
     */
    Fragment(final Element copy, final Map<String, String> leaves,
        final Map<String, String> voids, final Formas decided) {
        this.body = copy;
        this.names = leaves;
        this.formas = voids;
        this.tables = decided;
    }

    /**
     * Carve the fragment out of the copy, once.
     * @return TRUE when the fragment is worth lowering
     */
    boolean carved() {
        return this.harvested(this.body) >= 2 && !this.names.isEmpty();
    }

    /**
     * The carved body, voids referenced where the leaves stood.
     * @return The root element of the copy
     */
    Xnav fragment() {
        return new Xnav(this.body);
    }

    /**
     * The voids: names to formas, in declaration order.
     * @return The unmodifiable map
     */
    Map<String, String> voids() {
        return Collections.unmodifiableMap(this.formas);
    }

    /**
     * The leaf bases, in the order their voids are declared.
     * @return The unmodifiable collection
     */
    Collection<String> leaves() {
        return Collections.unmodifiableCollection(this.names.keySet());
    }

    private int harvested(final Element node) {
        final String base = node.getAttribute("base");
        final int out;
        if (Fragment.LITERALS.contains(base)) {
            out = 0;
        } else if (base.length() > 1 && base.charAt(0) == '.') {
            final int inner = this.gathered(node);
            if (inner < 0) {
                out = -1;
            } else {
                out = inner + 1;
            }
        } else if (base.startsWith("ξ") || base.startsWith("Φ.")) {
            out = this.leafed(node, base);
        } else {
            out = -1;
        }
        return out;
    }

    private int gathered(final Element node) {
        int out = 0;
        for (final Element kid : Fragment.kids(node)) {
            final int inner = this.harvested(kid);
            if (inner < 0) {
                out = -1;
                break;
            }
            out += inner;
        }
        return out;
    }

    private int leafed(final Element node, final String base) {
        String prefix = base;
        String method = "";
        String row = node.getAttribute("loc");
        int out = 0;
        if (!Fragment.kids(node).isEmpty()) {
            final int cut = base.lastIndexOf('.');
            prefix = base.substring(0, Math.max(cut, 0));
            method = base.substring(cut + 1);
            row = String.format("%s.ρ", row);
            final int inner = this.gathered(node);
            if (inner < 0) {
                out = -1;
            } else {
                out = inner + 1;
            }
        }
        if (out >= 0 && Fragment.leaf(prefix)) {
            final String name = this.named(prefix, this.tables.at(row));
            if (name.isEmpty()) {
                out = -1;
            } else if (method.isEmpty()) {
                node.setAttribute("base", String.format("ξ.%s", name));
            } else {
                node.setAttribute("base", String.format("ξ.%s.%s", name, method));
            }
        } else {
            out = -1;
        }
        return out;
    }

    private String named(final String prefix, final String forma) {
        String name = "";
        if (Fragment.CARRIERS.contains(forma)) {
            final String known = this.names.get(prefix);
            if (known == null) {
                name = String.format("v%d", this.names.size());
                this.names.put(prefix, name);
                this.formas.put(name, forma);
            } else if (forma.equals(this.formas.get(known))) {
                name = known;
            }
        }
        return name;
    }

    private static boolean leaf(final String prefix) {
        return !prefix.isEmpty() && !"ξ".equals(prefix) && !"Φ".equals(prefix)
            && !Fragment.LITERALS.contains(prefix);
    }

    private static List<Element> kids(final Element node) {
        final NodeList all = node.getChildNodes();
        final List<Element> out = new ArrayList<>(all.getLength());
        for (int idx = 0; idx < all.getLength(); ++idx) {
            final Node kid = all.item(idx);
            if (kid.getNodeType() == Node.ELEMENT_NODE && "o".equals(kid.getNodeName())) {
                out.add((Element) kid);
            }
        }
        return out;
    }
}
