/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import org.w3c.dom.Node;

/**
 * A copy of a document with every box taken back out.
 *
 * <p>It takes the document the runs of phino left behind and answers a
 * fresh copy without a single {@code <o name="λ">} holding the name of a
 * box. A box is how the build asks phino to fire the engine on a
 * formation; downstream nobody knows what one means, so none of them may
 * reach the file the build saves. One stylesheet does it, so no Java here
 * reads the document.</p>
 *
 * @since 0.77.0
 */
public final class Unboxed {

    /**
     * The document.
     */
    private final Node doc;

    /**
     * Ctor.
     *
     * @param xmir The document
     */
    public Unboxed(final Node xmir) {
        this.doc = xmir;
    }

    /**
     * The copy.
     *
     * @return A new document
     * @throws IOException If the stylesheet cannot be read
     */
    public Node copy() throws IOException {
        return new XSLDocument(
            Unboxed.class.getResource("/org/eolang/lowering/unboxing.xsl"), "unboxing.xsl"
        ).with("prefix", Lambda.PREFIX).transform(new XMLDocument(this.doc)).inner();
    }
}
