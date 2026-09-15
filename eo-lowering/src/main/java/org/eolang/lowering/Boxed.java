/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.util.stream.Collectors;
import org.w3c.dom.Node;

/**
 * A copy of a document with a box planted in every formation that takes
 * arguments.
 *
 * <p>It takes the document, the boxes of the build and the locator of the
 * one formation a run is lowering. It answers a fresh copy in which every
 * boxed formation carries an {@code <o name="λ">} element, so that
 * entering its body fires the engine, and in which the tests no fragment
 * reaches are cut away, so the universe phino reads stays small. One
 * stylesheet does all of it, so no Java here reads the document.</p>
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
    private final Boxes planted;

    /**
     * The locator of the formation being lowered, or blank.
     */
    private final String kept;

    /**
     * Ctor.
     *
     * @param xmir The document
     * @param boxes The boxes of the build
     * @param locator The locator of the formation being lowered, or blank
     */
    public Boxed(final Node xmir, final Boxes boxes, final String locator) {
        this.doc = xmir;
        this.planted = boxes;
        this.kept = locator;
    }

    /**
     * The copy.
     *
     * @return A new document
     * @throws IOException If the stylesheet cannot be read
     */
    public Node copy() throws IOException {
        return Boxed.sheet()
            .with("planted", this.rows())
            .with("kept", this.kept)
            .transform(new XMLDocument(this.doc))
            .inner();
    }

    private String rows() {
        return this.planted.all().stream()
            .map(box -> String.format("%s\t%s", box.locator(), box.lambda()))
            .collect(Collectors.joining(" "));
    }

    private static XSL sheet() throws IOException {
        return new XSLDocument(
            Boxed.class.getResource("/org/eolang/lowering/boxing.xsl"), "boxing.xsl"
        );
    }
}
