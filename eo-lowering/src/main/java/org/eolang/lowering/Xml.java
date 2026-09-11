/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;

/**
 * A DOM node spelled back as XML text, without a declaration and with
 * every character as it is, the emoji of the test attributes included,
 * which the transformers of the JDK would spell as character references.
 *
 * @since 0.77.0
 */
public final class Xml {

    /**
     * The node to spell.
     */
    private final Node node;

    /**
     * Ctor.
     *
     * @param root The node to spell
     */
    public Xml(final Node root) {
        this.node = root;
    }

    /**
     * The text.
     *
     * @return The XML
     */
    public String text() {
        final Document doc;
        if (this.node.getNodeType() == Node.DOCUMENT_NODE) {
            doc = (Document) this.node;
        } else {
            doc = this.node.getOwnerDocument();
        }
        final LSSerializer serializer = ((DOMImplementationLS) doc.getImplementation()
            .getFeature("LS", "3.0")).createLSSerializer();
        serializer.getDomConfig().setParameter("xml-declaration", Boolean.FALSE);
        return serializer.writeToString(this.node);
    }

    /**
     * Save the text into a file.
     *
     * @param target The file
     * @throws IOException If the file cannot be written
     */
    public void saved(final Path target) throws IOException {
        Files.createDirectories(target.toAbsolutePath().getParent());
        Files.write(target, this.text().getBytes(StandardCharsets.UTF_8));
    }
}
