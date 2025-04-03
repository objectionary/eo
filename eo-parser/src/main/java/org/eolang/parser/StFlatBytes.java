/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.xsline.StEnvelope;
import java.util.Optional;
import org.w3c.dom.Node;

/**
 * Shift that removes abstract object from the representation of bytes.
 * Before:
 * {@code
 *  &lt;o base="Q.org.eolang.bytes"&gt;&lt;o&gt;01-&lt;/o&gt;&lt;/o&gt;
 * }
 * After:
 * {@code
 *  &lt;o base="Q.org.eolang.bytes"&gt;01-&lt;/o&gt;
 * }
 * @since 0.55
 * @todo #4032:90min Remove the {@link StFlatBytes} usage from all the possible places.
 *  Currently we use this class to hide a problem with the XMIR of bytes.
 *  Some of the transformations were designed to work with the old XMIR format, where
 *  bytes were represented just as sequence of bytes within a 'Q.org.eolang.bytes' object.
 *  Now we have an abstract object that "wraps" the sequence of bytes.
 */
public final class StFlatBytes extends StEnvelope {

    /**
     * Ctor.
     */
    public StFlatBytes() {
        super(
            new StXnav(
                "st-flat-bytes",
                "//o[@base='Q.org.eolang.bytes']",
                xnav -> {
                    final Xnav element = xnav.element("o");
                    final Optional<String> text = element.text();
                    if (text.isPresent()) {
                        final Node elem = element.node();
                        final Node root = xnav.node();
                        root.removeChild(elem);
                        root.setTextContent(text.get());
                    }
                }
            )
        );
    }
}
