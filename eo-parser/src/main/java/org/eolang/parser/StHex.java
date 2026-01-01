/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StEnvelope;
import java.nio.ByteBuffer;
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
                        new BytesToHex(
                            ByteBuffer
                                .allocate(Double.BYTES)
                                .putDouble(Double.parseDouble(xnav.text().orElseThrow()))
                                .array()
                        ).get()
                    );
                    ((Element) node).removeAttribute("hex");
                }
            )
        );
    }
}
