/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Fold the constant fragments of one XMIR into literals.
 *
 * <p>A fragment qualifies when it is an application whose every leaf is
 * a literal, such as {@code 1.plus 1}: its value is decided by data
 * alone, so it is computed here, once, through {@code phino dataize},
 * and a literal of that value stands where the fragment stood. Which
 * methods can fold is phino's knowledge, not ours: every qualifying
 * fragment is simply tried, and one that dispatches outside the universe
 * fails there. Such a fragment is pure by construction — a literal
 * receiver dispatches into the standard library and nowhere else — so no
 * purity analysis is consulted. The walk is top-down and the outermost
 * qualifying fragment wins, since folding it folds everything inside
 * it.</p>
 *
 * <p>Folding is best-effort per fragment: whatever phino refuses — an
 * error-path expression, a method outside its tables, an exhausted
 * budget — stays as written, and the pass goes on.</p>
 *
 * @since 0.76.0
 */
public final class Folded implements Rewrite {

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     */
    public Folded(final Phino exe) {
        this.phino = exe;
    }

    @Override
    public int rewrite(final Xnav doc) throws IOException {
        final Collection<Xnav> found = new ArrayList<>(0);
        Folded.selected(doc.element("object").element("o"), found);
        int count = 0;
        for (final Xnav node : found) {
            if (this.spliced(node)) {
                ++count;
            }
        }
        return count;
    }

    private boolean spliced(final Xnav node) {
        boolean done;
        try {
            final Datum datum = new Constant(this.phino, node).value();
            Folded.carrier((Element) node.node(), datum.forma(), datum.bytes());
            done = true;
        } catch (final IllegalStateException | IOException ex) {
            done = false;
        }
        return done;
    }

    private static void carrier(final Element element, final String forma,
        final String value) {
        if ("number".equals(forma)) {
            if (value.length() != 23) {
                throw new IllegalStateException(
                    String.format("A number must dataize to eight bytes, not to '%s'", value)
                );
            }
            Folded.cleared(element);
            element.setAttribute("base", "Φ.number");
            element.appendChild(Folded.wrapped(element.getOwnerDocument(), value));
        } else if ("bool".equals(forma)) {
            if ("FF-".equals(value)) {
                element.setAttribute("base", "Φ.true");
            } else if ("00-".equals(value)) {
                element.setAttribute("base", "Φ.false");
            } else {
                throw new IllegalStateException(
                    String.format("A bool must dataize to one byte, not to '%s'", value)
                );
            }
            Folded.cleared(element);
        } else {
            Folded.cleared(element);
            element.setAttribute("base", "Φ.bytes");
            element.appendChild(Folded.datum(element.getOwnerDocument(), value));
        }
    }

    private static Element wrapped(final Document doc, final String value) {
        final Element out = doc.createElement("o");
        out.setAttribute("as", "α0");
        out.setAttribute("base", "Φ.bytes");
        out.appendChild(Folded.datum(doc, value));
        return out;
    }

    private static Element datum(final Document doc, final String value) {
        final Element out = doc.createElement("o");
        out.setAttribute("as", "α0");
        out.setTextContent(value);
        return out;
    }

    private static void cleared(final Element element) {
        while (element.getFirstChild() != null) {
            element.removeChild(element.getFirstChild());
        }
    }

    private static void selected(final Xnav node, final Collection<Xnav> out) {
        if (Folded.foldable(node)) {
            out.add(node);
        } else {
            for (final Xnav kid : Folded.kids(node)) {
                Folded.selected(kid, out);
            }
        }
    }

    private static boolean foldable(final Xnav node) {
        final String base = Folded.base(node);
        return base.length() > 1 && base.charAt(0) == '.'
            && Folded.decided(node);
    }

    private static boolean decided(final Xnav node) {
        boolean good = Folded.literal(node);
        if (!good) {
            final String base = Folded.base(node);
            if (base.length() > 1 && base.charAt(0) == '.') {
                final List<Xnav> kids = Folded.kids(node);
                good = !kids.isEmpty()
                    && kids.get(0).attribute("as").text().isEmpty();
                for (int idx = 1; good && idx < kids.size(); ++idx) {
                    good = kids.get(idx).attribute("as").text().isPresent();
                }
                for (int idx = 0; good && idx < kids.size(); ++idx) {
                    good = Folded.decided(kids.get(idx));
                }
            }
        }
        return good;
    }

    private static boolean literal(final Xnav node) {
        final String base = Folded.base(node);
        final List<Xnav> kids = Folded.kids(node);
        final boolean good;
        if ("Φ.true".equals(base) || "Φ.false".equals(base)) {
            good = kids.isEmpty();
        } else if ("Φ.bytes".equals(base)) {
            good = kids.size() == 1
                && Folded.base(kids.get(0)).isEmpty()
                && Folded.kids(kids.get(0)).isEmpty();
        } else if ("Φ.number".equals(base) || "Φ.string".equals(base)) {
            good = kids.size() == 1 && Folded.literal(kids.get(0));
        } else {
            good = false;
        }
        return good;
    }

    private static String base(final Xnav node) {
        return node.attribute("base").text().orElse("");
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
