/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * A self-contained φ-calculus document holding one XMIR fragment.
 *
 * <p>Dataizing a fragment needs the method tables of the primitive
 * λ-atoms it dispatches into, and phino resolves a {@code Φ.x} reference
 * against the root formation of the document it was given. So the
 * fragment is rendered as the {@code φ} of a root formation whose other
 * bindings are those tables, read from the {@code universe.phi} resource:
 * {@code Φ.number} in the fragment then finds the sibling that holds
 * {@code plus}, {@code times}, {@code div} and {@code gt}, and the
 * literal it carries lands in the {@code as-bytes} void the table
 * decorates. A dispatch into anything the tables do not hold leaves the
 * dataization stuck, which the caller reads as a refusal to fold.</p>
 *
 * <p>The rendering is a serialization of the XMIR subtree and nothing
 * more: dispatches become dotted applications, literal carriers become
 * applications of {@code Φ.number}, {@code Φ.string} or {@code Φ.bytes},
 * and the bare hex datum becomes a {@code Δ} formation. No phi syntax is
 * ever parsed here, and a subtree holding anything else — a void, a
 * {@code ξ} reference, a formation — is refused, since its meaning
 * depends on a context this document does not carry.</p>
 *
 * @since 0.76.0
 */
public final class MiniDoc {

    /**
     * The XMIR fragment to render, an {@code <o/>} element.
     */
    private final XML fragment;

    /**
     * Ctor.
     * @param xmir The XMIR fragment to render, an {@code <o/>} element
     */
    public MiniDoc(final XML xmir) {
        this.fragment = xmir;
    }

    /**
     * The document, in phi syntax.
     * @return The text for {@link Phino#dataize(String)}
     */
    public String text() {
        return String.format(
            "⟦%n%s  φ ↦ %s%n⟧%n",
            MiniDoc.tables(),
            MiniDoc.rendered(this.root())
        );
    }

    private Element root() {
        final Node node = this.fragment.inner();
        final Element found;
        if (node instanceof Document doc) {
            found = doc.getDocumentElement();
        } else {
            found = (Element) node;
        }
        return found;
    }

    private static String tables() {
        try (InputStream stream = MiniDoc.class.getResourceAsStream("universe.phi")) {
            return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        } catch (final IOException ex) {
            throw new IllegalStateException(
                "Failed to read universe.phi from classpath", ex
            );
        }
    }

    private static String rendered(final Element node) {
        final String base = node.getAttribute("base");
        final String out;
        if (base.isEmpty()) {
            out = String.format("⟦ Δ ⤍ %s ⟧", MiniDoc.datum(node));
        } else if (base.charAt(0) == '.') {
            out = MiniDoc.dispatched(node, base);
        } else if (base.startsWith("Φ.")) {
            out = MiniDoc.applied(base, MiniDoc.kids(node));
        } else {
            throw new IllegalStateException(
                String.format(
                    "The base '%s' depends on a context this document does not carry",
                    base
                )
            );
        }
        return out;
    }

    private static String dispatched(final Element node, final String base) {
        final List<Element> kids = MiniDoc.kids(node);
        if (kids.isEmpty()) {
            throw new IllegalStateException(
                String.format("The dispatch '%s' has no receiver", base)
            );
        }
        return String.format(
            "%s%s%s",
            MiniDoc.rendered(kids.get(0)),
            base,
            MiniDoc.arguments(kids.subList(1, kids.size()))
        );
    }

    private static String applied(final String base, final Collection<Element> kids) {
        final String out;
        if (kids.isEmpty()) {
            out = base;
        } else {
            out = String.format("%s%s", base, MiniDoc.arguments(kids));
        }
        return out;
    }

    private static String arguments(final Collection<Element> kids) {
        final String out;
        if (kids.isEmpty()) {
            out = "";
        } else {
            final Collection<String> parts = new ArrayList<>(kids.size());
            for (final Element kid : kids) {
                final String name = kid.getAttribute("as");
                if (name.isEmpty()) {
                    throw new IllegalStateException(
                        "An argument without a binding name cannot be rendered"
                    );
                }
                parts.add(
                    String.format("%s ↦ %s", name, MiniDoc.rendered(kid))
                );
            }
            out = String.format("(%s)", String.join(", ", parts));
        }
        return out;
    }

    private static String datum(final Element node) {
        return node.getTextContent().replaceAll("\\s+", "");
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
