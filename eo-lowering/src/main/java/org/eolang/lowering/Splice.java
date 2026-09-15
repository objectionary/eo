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
 * The residual of a run put back into the formation it came from.
 *
 * <p>It takes the formation as written and the one phino printed. It
 * copies over only the bindings phino reduced whole — those spelled with
 * markers and literals alone — and leaves everything else exactly as
 * written, with its locators, lines, positions and local names, since
 * phino spells the rest afresh and drops what the printer lives on.</p>
 *
 * @since 0.77.0
 * @todo #8548:90min Merge the residual without reading XMIR from Java.
 *  This class, together with Sites, Route and Marked, is all that still
 *  reads and writes XMIR from Java: boxing, unboxing, planting and the
 *  way into a fragment are stylesheets already. Taking the output of
 *  phino verbatim instead was probed on 0.0.132 and refused twice. A
 *  morph of the whole boxed world reduces nothing, because a formation
 *  with a free void is already a normal form and nothing in a library
 *  applies it, so only the symbolic application Applied builds makes the
 *  engine fire at all. A morph of that whole application answers only
 *  the value of its φ, dropping every binding the engine cannot reduce,
 *  which would delete code from the program. So the residual comes back
 *  one binding at a time and somebody has to merge it. Turn that merge
 *  and the marking into stylesheets, leaving Java only the sidecars it
 *  writes by digest, or ask phino for a morph that answers a formation
 *  with its irreducible bindings kept.
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
