/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;

/**
 * What is noted on one node of a document.
 *
 * <p>A rule reads a great many nodes and asks each of them for two or three of
 * its attributes. Asking a document for an attribute is a question about the
 * whole document and costs what the whole document costs, however small the
 * answer is, so a node asked three times over forty thousand nodes is the most
 * expensive thing a rule does. Reading the attribute off the node itself costs
 * what the node costs, which is nothing.</p>
 *
 * <p>An attribute that is not there comes back empty rather than as a
 * complaint. Every reader here already treats a missing attribute as a fact
 * about the object rather than as a fault of the table.</p>
 *
 * @since 0.71.0
 */
final class Noted {

    /**
     * The node.
     */
    private final Xnav node;

    /**
     * Ctor.
     * @param element The node
     */
    Noted(final XML element) {
        this(new Xnav(element.inner()));
    }

    /**
     * Ctor.
     * @param element The node
     */
    Noted(final Xnav element) {
        this.node = element;
    }

    /**
     * What the node says about one of its attributes.
     * @param attribute The name of the attribute
     * @return What it says, empty when it says nothing
     */
    String says(final String attribute) {
        return this.node.attribute(attribute).text().orElse("");
    }
}
