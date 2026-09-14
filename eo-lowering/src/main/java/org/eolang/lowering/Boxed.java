/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * A copy of an XMIR document with a box planted in every boxed formation,
 * and every test attribute trimmed unless the kept formation stands
 * inside it.
 *
 * <p>The box is one {@code <o name="λ">} element appended to the
 * formation, which phino reads as the λ of it, so entering the body
 * fires the engine. The kept formation is the one a run lowers: the run
 * dispatches into its bindings through a copy of it, which fires nothing,
 * while an entry into another copy of it, which is what a recursive call
 * makes, fires the box like any other. A formation lowered by an earlier
 * run carries the λ of its atom already,
 * and the box takes the place of that λ, since the engine serves boxes
 * and not atoms. A test is a thunk over literals and no fragment reaches
 * it, so it only weighs the universe down.</p>
 *
 * @since 0.77.0
 */
public final class Boxed {

    /**
     * The document.
     */
    private final Node doc;

    /**
     * The boxes of the build.
     */
    private final Boxes boxes;

    /**
     * The locator of the formation left unboxed, or blank.
     */
    private final String kept;

    /**
     * Ctor.
     *
     * @param xmir The document
     * @param planted The boxes of the build
     * @param locator The locator of the formation left unboxed, or blank
     */
    public Boxed(final Node xmir, final Boxes planted, final String locator) {
        this.doc = xmir;
        this.boxes = planted;
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
            Boxed.planted(node, this.boxes.of(node.getAttribute("loc")));
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

    private static void planted(final Element node, final String lambda) {
        if (!lambda.isEmpty()) {
            new Kids(node).all().stream()
                .filter(kid -> "λ".equals(kid.getAttribute("name")))
                .forEach(node::removeChild);
            final Element box = node.getOwnerDocument().createElement("o");
            box.setAttribute("name", "λ");
            box.setTextContent(lambda);
            node.appendChild(box);
        }
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
