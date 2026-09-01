/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eolang.lowering.Constant;
import org.eolang.lowering.Phino;
import org.eolang.lowering.Primitive;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Fold the constant fragments of every XMIR this build compiles.
 *
 * <p>A fragment qualifies when it is an application whose every dispatch
 * is one of the twelve primitive methods and whose every leaf is a
 * literal, such as {@code 1.plus 1}: its value is decided by data alone,
 * so it is computed here, once, through {@code phino dataize}, and a
 * literal of that value stands where the fragment stood. Such a fragment
 * is pure by construction — a literal receiver dispatches into the
 * standard library and nowhere else — so no purity analysis is consulted.
 * The walk is top-down and the outermost qualifying fragment wins, since
 * folding it folds everything inside it.</p>
 *
 * <p>Folding is best-effort per fragment: whatever phino refuses — an
 * error-path expression, a method outside its tables, an exhausted
 * budget — stays as written, and the build goes on. A file with nothing
 * folded is neither rewritten nor repointed, so a build without foldable
 * fragments leaves no trace of this step at all.</p>
 *
 * @since 0.76.0
 */
final class Lowering implements Step {

    /**
     * The directory for the folded XMIR.
     */
    static final String DIR = "4-lower";

    /**
     * The file that says lowering ran, and with what.
     */
    static final String MARKER = "lowering.txt";

    /**
     * XMIR sources to fold.
     */
    private final Collection<TjForeign> sources;

    /**
     * The directory to write the folded XMIR to.
     */
    private final Path home;

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * Ctor.
     * @param srcs XMIR sources to fold
     * @param target The directory for the folded XMIR
     * @param exe The binary that dataizes
     */
    Lowering(final Collection<TjForeign> srcs, final Path target, final Phino exe) {
        this.sources = srcs;
        this.home = target;
        this.phino = exe;
    }

    @Override
    public void exec() throws IOException {
        Logger.info(
            this, "Folded %d constant fragment(s) in %d XMIR(s), into %[file]s",
            new Threaded<>(this.sources, this::folded).total(),
            this.sources.size(), this.home
        );
    }

    private int folded(final TjForeign tojo) throws IOException {
        final XMLDocument doc = new XMLDocument(tojo.xmir());
        final Collection<Element> found = new ArrayList<>(0);
        Lowering.selected(
            (Element) new Xnav(doc.inner()).element("object").element("o").node(),
            found
        );
        int count = 0;
        for (final Element node : found) {
            if (this.spliced(node)) {
                ++count;
            }
        }
        if (count > 0) {
            final Path target = new Place(tojo.identifier())
                .make(this.home, MjAssemble.XMIR);
            new Saved(doc.toString(), target).value();
            tojo.withXmir(target);
        }
        return count;
    }

    private boolean spliced(final Element node) {
        boolean done = false;
        try {
            final Constant constant = new Constant(this.phino, new XMLDocument(node));
            final Element literal = Lowering.carrier(
                node.getOwnerDocument(), constant.forma(), constant.value()
            );
            for (final String name : new String[] {"as", "name", "line", "pos"}) {
                final String value = node.getAttribute(name);
                if (!value.isEmpty()) {
                    literal.setAttribute(name, value);
                }
            }
            node.getParentNode().replaceChild(literal, node);
            done = true;
        } catch (final IllegalStateException | IOException ex) {
            Logger.debug(this, "A fragment stays unfolded: %s", ex.getMessage());
        }
        return done;
    }

    private static Element carrier(
        final Document doc, final String forma, final String value
    ) {
        final Element made;
        if ("number".equals(forma)) {
            if (value.length() != 23) {
                throw new IllegalStateException(
                    String.format("A number must dataize to eight bytes, not to '%s'", value)
                );
            }
            made = doc.createElement("o");
            made.setAttribute("base", "Φ.number");
            made.appendChild(Lowering.bytes(doc, value, "α0"));
        } else if ("bool".equals(forma)) {
            made = doc.createElement("o");
            if ("01-".equals(value)) {
                made.setAttribute("base", "Φ.true");
            } else if ("00-".equals(value)) {
                made.setAttribute("base", "Φ.false");
            } else {
                throw new IllegalStateException(
                    String.format("A bool must dataize to one byte, not to '%s'", value)
                );
            }
        } else {
            made = Lowering.bytes(doc, value, "");
        }
        return made;
    }

    private static Element bytes(
        final Document doc, final String value, final String bound
    ) {
        final Element made = doc.createElement("o");
        made.setAttribute("base", "Φ.bytes");
        if (!bound.isEmpty()) {
            made.setAttribute("as", bound);
        }
        final Element datum = doc.createElement("o");
        datum.setAttribute("as", "α0");
        datum.setTextContent(value);
        made.appendChild(datum);
        return made;
    }

    private static void selected(final Element node, final Collection<Element> out) {
        if (Lowering.foldable(node)) {
            out.add(node);
        } else {
            for (final Element kid : Lowering.kids(node)) {
                Lowering.selected(kid, out);
            }
        }
    }

    private static boolean foldable(final Element node) {
        final String base = node.getAttribute("base");
        return base.length() > 1 && base.charAt(0) == '.'
            && new Primitive(base.substring(1)).known()
            && Lowering.decided(node);
    }

    private static boolean decided(final Element node) {
        boolean good = Lowering.literal(node);
        if (!good) {
            final String base = node.getAttribute("base");
            if (base.length() > 1 && base.charAt(0) == '.'
                && new Primitive(base.substring(1)).known()) {
                final List<Element> kids = Lowering.kids(node);
                good = !kids.isEmpty()
                    && kids.get(0).getAttribute("as").isEmpty();
                for (int idx = 1; good && idx < kids.size(); ++idx) {
                    good = !kids.get(idx).getAttribute("as").isEmpty();
                }
                for (int idx = 0; good && idx < kids.size(); ++idx) {
                    good = Lowering.decided(kids.get(idx));
                }
            }
        }
        return good;
    }

    private static boolean literal(final Element node) {
        final String base = node.getAttribute("base");
        final List<Element> kids = Lowering.kids(node);
        final boolean good;
        if ("Φ.true".equals(base) || "Φ.false".equals(base)) {
            good = kids.isEmpty();
        } else if ("Φ.bytes".equals(base)) {
            good = kids.size() == 1
                && kids.get(0).getAttribute("base").isEmpty()
                && Lowering.kids(kids.get(0)).isEmpty();
        } else if ("Φ.number".equals(base) || "Φ.string".equals(base)) {
            good = kids.size() == 1 && Lowering.literal(kids.get(0));
        } else {
            good = false;
        }
        return good;
    }

    private static List<Element> kids(final Element node) {
        final NodeList nodes = node.getChildNodes();
        final List<Element> found = new ArrayList<>(nodes.getLength());
        for (int idx = 0; idx < nodes.getLength(); ++idx) {
            final Node kid = nodes.item(idx);
            if (kid.getNodeType() == Node.ELEMENT_NODE) {
                found.add((Element) kid);
            }
        }
        return found;
    }
}
