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
 * <p>Turning XMIR into phi is phino's own job, reached with
 * {@code --input=xmir}, and this class only asks for it: the fragment is
 * wrapped into the document shape that reader expects — an
 * {@code <object/>} holding one {@code <o/>} named {@code φ} — and
 * {@link Phino#phi(String)} prints it back in phi syntax. No phi is
 * written here, so nothing on this side has to know how a dispatch, a
 * literal carrier or a datum spells out; when the dialect moves, the
 * pinned binary moves with it and this class does not.</p>
 *
 * <p>The copy handed over is bound to {@code φ} of the root formation,
 * which is what a {@code name} attribute means to the XMIR reader, and
 * loses the {@code as} of the site it was carved from, since a binding
 * of the root belongs to no application.</p>
 *
 * <p>What comes back is a formation binding the fragment to {@code φ}, a
 * complete expression of its own. It is not evaluatable alone, though:
 * the {@code Φ.number} and {@code Φ.bytes} references inside it resolve
 * only when {@code phino merge} joins this expression with the
 * {@link Universe}, whose root formation holds those method tables. A
 * fragment whose meaning depends on a context this document does not
 * carry — a {@code ξ} reference to a void declared elsewhere — renders
 * like any other and is refused later, by the dataization that walks
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
        root.removeAttribute("as");
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
