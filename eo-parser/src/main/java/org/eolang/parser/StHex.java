/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StEnvelope;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * This {@link Shift} turns regular data (numbers) inside XMIR
 * into hexadecimal representation.
 *
 * @since 0.54
 */
final class StHex extends StEnvelope {

    /**
     * Ctor.
     */
    StHex() {
        super(
            new StXnav(
                "st-hex",
                "//o[@hex]",
                xnav -> {
                    final Node node = xnav.node();
                    node.setTextContent(
                        new Hex(
                            Double.parseDouble(xnav.text().orElseThrow())
                        ).asString()
                    );
                    ((Element) node).removeAttribute("hex");
                }
            )
        );
    }
}
