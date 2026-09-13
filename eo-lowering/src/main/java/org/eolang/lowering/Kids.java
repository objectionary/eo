/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The {@code o} elements directly under a node of an XMIR document, in
 * order, read once when the iteration starts, so the caller may remove or
 * replace them while walking.
 *
 * @since 0.77.0
 */
final class Kids implements Iterable<Element> {

    /**
     * The node whose children are listed.
     */
    private final Node parent;

    /**
     * Ctor.
     *
     * @param node The node whose children are listed
     */
    Kids(final Node node) {
        this.parent = node;
    }

    @Override
    public Iterator<Element> iterator() {
        return this.all().iterator();
    }

    /**
     * All of them, as a list of their own.
     *
     * @return The elements, in document order
     */
    List<Element> all() {
        final NodeList nodes = this.parent.getChildNodes();
        final List<Element> out = new ArrayList<>(nodes.getLength());
        for (int idx = 0; idx < nodes.getLength(); ++idx) {
            final Node kid = nodes.item(idx);
            if (kid.getNodeType() == Node.ELEMENT_NODE && "o".equals(kid.getNodeName())) {
                out.add((Element) kid);
            }
        }
        return out;
    }
}
