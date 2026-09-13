/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.w3c.dom.Element;

/**
 * The element of an XMIR document at a locator.
 *
 * @since 0.77.0
 */
final class Located {

    /**
     * The root to look under.
     */
    private final Element root;

    /**
     * The locator.
     */
    private final String place;

    /**
     * Ctor.
     *
     * @param node The root to look under
     * @param locator The locator
     */
    Located(final Element node, final String locator) {
        this.root = node;
        this.place = locator;
    }

    /**
     * The element.
     *
     * @return The element with that locator
     */
    Element element() {
        final Element out = Located.found(this.root, this.place);
        if (out == null) {
            throw new IllegalStateException(
                String.format("The document has no element at '%s'", this.place)
            );
        }
        return out;
    }

    private static Element found(final Element root, final String place) {
        Element out = null;
        for (final Element kid : new Kids(root)) {
            if (place.equals(kid.getAttribute("loc"))) {
                out = kid;
            } else if (place.startsWith(String.format("%s.", kid.getAttribute("loc")))) {
                out = Located.found(kid, place);
            }
            if (out != null) {
                break;
            }
        }
        return out;
    }
}
