/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Set;
import java.util.stream.Collectors;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * A copy of a document with a box planted in every formation that takes
 * arguments.
 *
 * <p>It takes the document, the locators of the boxed formations and the
 * locator of the one formation a run is lowering. It answers a fresh copy
 * in which every boxed formation carries an {@code <o name="λ">} element,
 * so that entering its body fires the engine, and in which the tests no
 * fragment reaches are cut away, so the universe phino reads stays
 * small.</p>
 *
 * @since 0.77.0
 */
public final class Boxed {

    /**
     * The document.
     */
    private final Node doc;

    /**
     * The locators of the boxed formations.
     */
    private final Set<String> planted;

    /**
     * The locator of the formation left unboxed, or blank.
     */
    private final String kept;

    /**
     * Ctor.
     *
     * @param xmir The document
     * @param boxes The boxes of the build
     * @param locator The locator of the formation left unboxed, or blank
     */
    public Boxed(final Node xmir, final Boxes boxes, final String locator) {
        this(
            xmir,
            boxes.all().stream().map(Box::locator).collect(Collectors.toSet()),
            locator
        );
    }

    /**
     * Ctor.
     *
     * @param xmir The document
     * @param places The locators of the boxed formations
     * @param locator The locator of the formation left unboxed, or blank
     */
    Boxed(final Node xmir, final Set<String> places, final String locator) {
        this.doc = xmir;
        this.planted = places;
        this.kept = locator;
    }

    /**
     * The copy.
     *
     * @return A new document
     */
    public Document copy() {
        final Document out = (Document) Boxed.owner(this.doc).cloneNode(true);
        for (final Element top : new Kids(out.getDocumentElement())) {
            this.through(top);
        }
        return out;
    }

    private void through(final Element node) {
        final boolean formation = !node.hasAttribute("base") && node.hasAttribute("name");
        if (formation && this.trimmed(node)) {
            node.getParentNode().removeChild(node);
        } else if (formation) {
            final String place = node.getAttribute("loc");
            if (this.planted.contains(place)) {
                Boxed.boxed(node, new Lambda(place).name());
            }
            for (final Element kid : new Kids(node)) {
                this.through(kid);
            }
        }
    }

    private boolean trimmed(final Element node) {
        final String name = node.getAttribute("name");
        return (name.startsWith("p🌵") || name.startsWith("n🌵"))
            && !this.kept.startsWith(String.format("%s.", node.getAttribute("loc")));
    }

    private static void boxed(final Element node, final String lambda) {
        new Kids(node).all().stream()
            .filter(kid -> "λ".equals(kid.getAttribute("name")))
            .forEach(node::removeChild);
        final Element box = node.getOwnerDocument().createElement("o");
        box.setAttribute("name", "λ");
        box.setTextContent(lambda);
        node.appendChild(box);
    }

    private static Node owner(final Node node) {
        final Node out;
        if (node.getNodeType() == Node.DOCUMENT_NODE) {
            out = node;
        } else {
            out = node.getOwnerDocument();
        }
        return out;
    }
}
