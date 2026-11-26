/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import java.util.Map;
import java.util.regex.Pattern;
import org.cactoos.Scalar;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The XMIR of the parser, with every argument named after the void it
 * lands in, as the goal {@code eo:dealpha} names them in a build.
 *
 * <p>The parser names every argument by its place: {@code α0},
 * {@code α1}, and so on. Phino reads no such name, so a test that gives
 * phino the XMIR of the parser must name each argument first. The bytes of
 * a literal go to the void {@code φ} of {@code Φ.number},
 * {@code Φ.string} or {@code Φ.bytes}, so this object names them so by
 * itself. Every other argument takes the name the test gives for its
 * {@code @loc}. The {@code @loc} stays as it is.</p>
 *
 * @since 0.69.0
 */
final class Dealphaed implements Scalar<String> {

    /**
     * The positional name of an argument.
     */
    private static final Pattern ALPHA = Pattern.compile("^α[0-9]+$");

    /**
     * The XMIR the parser wrote.
     */
    private final String xmir;

    /**
     * The names of the voids the arguments land in, by their locators.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     *
     * @param src The XMIR the parser wrote
     * @param lands The names of the voids the arguments land in, by their
     *  locators
     */
    Dealphaed(final String src, final Map<String, String> lands) {
        this.xmir = src;
        this.names = lands;
    }

    @Override
    public String value() {
        final Node root = new XMLDocument(this.xmir).inner();
        Dealphaed.walked(root, this.names);
        return new XMLDocument(root).toString();
    }

    /**
     * Name every positional argument under this node.
     *
     * @param node The node to walk
     * @param names The names of the voids, by locators
     */
    private static void walked(final Node node, final Map<String, String> names) {
        final NodeList kids = node.getChildNodes();
        for (int idx = 0; idx < kids.getLength(); ++idx) {
            final Node kid = kids.item(idx);
            if (kid instanceof Element) {
                Dealphaed.named((Element) kid, node, names);
                Dealphaed.walked(kid, names);
            }
        }
    }

    /**
     * Name one argument, if its name is positional.
     *
     * @param kid The argument
     * @param parent The application it belongs to
     * @param names The names of the voids, by locators
     */
    private static void named(
        final Element kid, final Node parent, final Map<String, String> names
    ) {
        if (Dealphaed.ALPHA.matcher(kid.getAttribute("as")).matches()) {
            final String loc = kid.getAttribute("loc");
            if (names.containsKey(loc)) {
                kid.setAttribute("as", names.get(loc));
            } else if (parent instanceof Element
                && ((Element) parent).getAttribute("base").matches("Φ\\.(number|string|bytes)")) {
                kid.setAttribute("as", "φ");
            } else {
                throw new IllegalArgumentException(
                    String.format("No name is given for the argument at '%s'", loc)
                );
            }
        }
    }
}
