/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import org.w3c.dom.Element;

/**
 * The residual of a run spliced back into the formation it came from: each
 * binding phino reduced to a marker stands in place of the one written,
 * while the voids, the λ, the shape of the nested formations and every
 * binding reduced only in part stay as written, with their locators,
 * lines, positions and local names.
 *
 * <p>phino prints the residual without ρ, without positions and with a
 * box on every nested formation, none of which belongs in the document,
 * and it spells afresh whatever it did not reduce, dropping the names and
 * places the printer lives on. So the walk
 * keeps the original and imports only a binding that reduced whole: one
 * spelled with markers, literals and their carriers alone, at every depth
 * of the nested formations.</p>
 *
 * @since 0.77.0
 */
final class Splice {

    /**
     * The shape of a symbol phino leaves behind.
     */
    private static final Pattern SYMBOL = Pattern.compile("S\\d+");

    /**
     * The bases a marker or a literal is spelled with, and nothing else.
     */
    private static final Set<String> CARRIERS = new HashSet<>(
        Arrays.asList(
            "∅", "Φ.number", "Φ.string", "Φ.bytes", "Φ.bool", "Φ.true", "Φ.false", "Φ.tuple"
        )
    );

    /**
     * The formation as written.
     */
    private final Element original;

    /**
     * The residual of the run.
     */
    private final Element residual;

    /**
     * Ctor.
     *
     * @param written The formation as written
     * @param reduced The residual of the run
     */
    Splice(final Element written, final Element reduced) {
        this.original = written;
        this.residual = reduced;
    }

    /**
     * Splice.
     */
    void apply() {
        Splice.merged(this.original, this.residual);
    }

    private static void merged(final Element written, final Element reduced) {
        final Map<String, Element> answers = new LinkedHashMap<>(0);
        for (final Element kid : new Kids(reduced)) {
            answers.put(kid.getAttribute("name"), kid);
        }
        for (final Element kid : new Kids(written)) {
            final Element answer = answers.get(kid.getAttribute("name"));
            if (answer != null && Splice.open(kid)) {
                if (kid.hasAttribute("base")) {
                    Splice.replaced(kid, answer);
                } else {
                    Splice.merged(kid, answer);
                }
            }
        }
    }

    private static boolean open(final Element kid) {
        return !"λ".equals(kid.getAttribute("name")) && !"∅".equals(kid.getAttribute("base"));
    }

    private static void replaced(final Element kid, final Element answer) {
        if (Splice.answered(answer) && Splice.marked(answer)) {
            final Element imported = (Element) kid.getOwnerDocument().importNode(answer, true);
            for (final String attr : new String[] {"loc", "line", "pos", "local"}) {
                if (kid.hasAttribute(attr)) {
                    imported.setAttribute(attr, kid.getAttribute(attr));
                }
            }
            kid.getParentNode().replaceChild(imported, kid);
        }
    }

    private static boolean answered(final Element node) {
        boolean out = !node.hasAttribute("base")
            || Splice.CARRIERS.contains(node.getAttribute("base"));
        for (final Element kid : new Kids(node)) {
            out = out && Splice.answered(kid);
        }
        return out;
    }

    private static boolean marked(final Element node) {
        boolean out = false;
        for (final Element kid : new Kids(node)) {
            if ("λ".equals(kid.getAttribute("name"))
                && Splice.SYMBOL.matcher(kid.getTextContent()).matches()
                || Splice.marked(kid)) {
                out = true;
                break;
            }
        }
        return out;
    }
}
