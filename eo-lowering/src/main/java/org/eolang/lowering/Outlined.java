/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Rewrite the pure applications of one XMIR into calls of synthetic atoms.
 *
 * <p>An application qualifies when everything under it is a literal, a
 * reference whose locator the tables of {@code eo:inference} decide as
 * number or bytes, or such an application again — so its value is a
 * straight-line computation over data inputs. This is the constructive
 * purity check of {@code purify.xsl}, taken one step further: where the
 * stylesheet stops at any part that is itself an application, the walk
 * here descends into it, so a whole arithmetic expression qualifies at
 * once. The subtree is carved into a {@link Fragment} — every reference
 * leaf becomes a void — and goes through the same mill as the pure
 * formations of {@link Lowered}: reduced into a protocol, rendered into
 * a sidecar Java body, marked with {@code λ}. The fragment lands as a
 * new formation attribute of the nearest enclosing formation, named
 * after the digest of its own body, and the application is rewritten in
 * place into a call of that attribute with the original leaves as
 * arguments — so at run time the whole subtree is one atom instead of a
 * chain of dispatches. Two sites with the same digest under one host
 * share one attribute.</p>
 *
 * <p>The walk is top-down and the outermost qualifying application wins;
 * when it refuses, its parts get their own try. Every refusal — an
 * undecided leaf, a foreign operation, a one-step computation whose op
 * is a Java atom already, an unnamed enclosing formation, which the
 * transpile chain hoists in a way {@code lowered.xsl} refuses — leaves
 * the application as written, the way {@link Lowered} treats its own
 * refusals.</p>
 *
 * @since 0.76.0
 */
public final class Outlined implements Rewrite {

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * The tables that decide the forma of a leaf.
     */
    private final Formas formas;

    /**
     * The directory for the sidecar bodies.
     */
    private final Path atoms;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param tables The tables that decide the forma of a leaf
     * @param home The directory for the sidecar bodies
     */
    public Outlined(final Phino exe, final Formas tables, final Path home) {
        this.phino = exe;
        this.formas = tables;
        this.atoms = home;
    }

    @Override
    public int rewrite(final Xnav doc) throws IOException {
        int count = 0;
        if (!this.formas.blank()) {
            count = this.walked(doc.element("object"));
        }
        return count;
    }

    private int walked(final Xnav node) throws IOException {
        int count = 0;
        for (final Xnav kid : Outlined.kids(node)) {
            if (this.risen((Element) kid.node())) {
                ++count;
            } else {
                count += this.walked(kid);
            }
        }
        return count;
    }

    private boolean risen(final Element site) throws IOException {
        final Element host = Outlined.host(site);
        boolean done = false;
        if (host != null && site.hasAttribute("base") && site.hasAttribute("loc")) {
            final Fragment cut = new Fragment(site, this.formas);
            done = cut.carved() && this.spliced(site, host, cut);
        }
        return done;
    }

    private boolean spliced(final Element site, final Element host,
        final Fragment cut) throws IOException {
        String text = "";
        String carrier = "";
        try {
            final Protocol protocol =
                new Reduction(this.phino, cut.fragment(), cut.voids(), 8).protocol();
            if (protocol.moves().size() >= 2) {
                text = new JavaAtom(protocol, cut.voids()).text();
                carrier = protocol.carrier();
            }
        } catch (final IllegalStateException | IOException ex) {
            carrier = "";
        }
        final boolean done = !carrier.isEmpty();
        if (done) {
            final String digest = new Sidecar(this.atoms, text).save();
            final String name = String.format("l🌵%s", digest);
            if (!Outlined.bound(host, name)) {
                host.appendChild(Outlined.formation(host, name, digest, cut, carrier));
            }
            Outlined.called(site, name, cut);
        }
        return done;
    }

    private static Element formation(final Element host, final String name,
        final String digest, final Fragment cut, final String carrier) {
        final Document doc = host.getOwnerDocument();
        final Element out = doc.createElement("o");
        out.setAttribute("name", name);
        out.setAttribute("loc", String.format("%s.%s", host.getAttribute("loc"), name));
        out.setAttribute(
            "original-name",
            String.format("%s.%s", host.getAttribute("original-name"), name)
        );
        out.setAttribute("pure", "true");
        out.setAttribute("lowered", digest);
        for (final Map.Entry<String, String> entry : cut.voids().entrySet()) {
            final Element vain = doc.createElement("o");
            vain.setAttribute("base", "∅");
            vain.setAttribute("name", entry.getKey());
            vain.setAttribute("type", String.format("Φ.%s", entry.getValue()));
            out.appendChild(vain);
        }
        final Element marker = doc.createElement("o");
        marker.setAttribute("name", "λ");
        marker.setAttribute("atom", String.format("Φ.%s", carrier));
        out.appendChild(marker);
        return out;
    }

    private static void called(final Element site, final String name, final Fragment cut) {
        site.setAttribute("base", String.format("ξ.%s", name));
        while (site.getFirstChild() != null) {
            site.removeChild(site.getFirstChild());
        }
        final Document doc = site.getOwnerDocument();
        int idx = 0;
        for (final String leaf : cut.leaves()) {
            final Element argument = doc.createElement("o");
            argument.setAttribute("as", String.format("α%d", idx));
            argument.setAttribute("base", leaf);
            site.appendChild(argument);
            ++idx;
        }
    }

    private static boolean bound(final Element host, final String name) {
        return new Xnav(host).elements(Filter.withName("o")).anyMatch(
            kid -> name.equals(kid.attribute("name").text().orElse(""))
        );
    }

    private static Element host(final Element site) {
        Element found = null;
        if (Outlined.grounded(site)) {
            Node cursor = site.getParentNode();
            while (cursor != null && cursor.getNodeType() == Node.ELEMENT_NODE
                && "o".equals(cursor.getNodeName())) {
                final Element parent = (Element) cursor;
                if (!parent.hasAttribute("base")) {
                    found = parent;
                    break;
                }
                cursor = cursor.getParentNode();
            }
        }
        return found;
    }

    private static boolean grounded(final Element site) {
        boolean named = true;
        Node cursor = site.getParentNode();
        while (cursor != null && cursor.getNodeType() == Node.ELEMENT_NODE
            && "o".equals(cursor.getNodeName())) {
            final Element parent = (Element) cursor;
            if (!parent.hasAttribute("base") && parent.getAttribute("name").isEmpty()) {
                named = false;
                break;
            }
            cursor = cursor.getParentNode();
        }
        return named;
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
