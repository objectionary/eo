/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.io.StringWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Element;

/**
 * One XMIR fragment as a φ-calculus expression.
 *
 * <p>The fragment is rendered as a formation binding it to {@code φ}, a
 * complete expression of its own. It is not evaluatable alone, though:
 * the {@code Φ.number} and {@code Φ.bytes} references inside it resolve
 * only when {@code phino merge} joins this expression with the
 * {@link Universe}, whose root formation holds those method tables.</p>
 *
 * <p>The rendering is phino's own, reached with {@code --input=xmir}:
 * this class only wraps the fragment into the document that reader
 * expects — an {@code <object/>} holding one {@code <o/>} named
 * {@code φ}, every other attribute of it ignored — and
 * {@link Phino#phi(String)} prints it. No phi syntax is written here, so
 * a subtree whose meaning depends on a context this document does not
 * carry — a {@code ξ} reference to a void declared elsewhere — renders
 * like any other, and is refused later by the dataization that walks
 * into the error terminator.</p>
 *
 * @since 0.76.0
 */
public final class Expression {

    /**
     * The binary that reads XMIR.
     */
    private final Phino phino;

    /**
     * The XMIR fragment to render, an {@code <o/>} element.
     */
    private final Xnav fragment;

    /**
     * Ctor.
     * @param exe The binary that reads XMIR
     * @param xmir The XMIR fragment to render, an {@code <o/>} element
     */
    public Expression(final Phino exe, final Xnav xmir) {
        this.phino = exe;
        this.fragment = xmir;
    }

    /**
     * The expression, in phi syntax.
     * @return The text for {@link Phino#dataize(String...)}
     * @throws IOException If the binary cannot be run
     */
    public String text() throws IOException {
        return this.phino.phi(this.document());
    }

    private String document() {
        final Element root = (Element) this.fragment.node().cloneNode(true);
        root.setAttribute("name", "φ");
        final StringWriter writer = new StringWriter();
        try {
            final Transformer transformer = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            transformer.transform(new DOMSource(root), new StreamResult(writer));
        } catch (final TransformerException ex) {
            throw new IllegalStateException(
                "Failed to print the XMIR fragment for phino", ex
            );
        }
        return String.format("<object>%s</object>", writer);
    }
}
