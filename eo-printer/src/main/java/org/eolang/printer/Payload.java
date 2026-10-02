/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.List;
import java.util.stream.Collectors;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The hex byte payload of a data literal in XMIR.
 *
 * <p>The payload is the {@code α0} argument of the literal's wrapper element,
 * and {@link StUnhex} folds it into the readable EO text of the same value. A
 * bare data literal can be the head of an application, as in {@code 42 5}, and
 * then the wrapper holds the applied argument next to the payload. The applied
 * arguments are taken out before the wrapper is emptied and put back after,
 * which also drops the indentation between them, and they take the places left
 * by the payload, since the slot they were counted from is gone.</p>
 *
 * @since 0.73.3
 */
final class Payload {

    /**
     * The wrapper element of the literal.
     */
    private final Xnav literal;

    /**
     * Ctor.
     *
     * @param wrapper The {@code <o>} element of the literal
     */
    Payload(final Xnav wrapper) {
        this.literal = wrapper;
    }

    /**
     * Put the given text in place of the payload.
     *
     * @param text The readable EO text of the literal
     */
    void replace(final String text) {
        final List<Node> args = this.literal.elements(Filter.withName("o"))
            .skip(1)
            .map(Xnav::node)
            .collect(Collectors.toList());
        final Node wrapper = this.literal.node();
        wrapper.setTextContent(text);
        int index = 0;
        for (final Node arg : args) {
            final Element elem = (Element) arg;
            if (elem.getAttribute("as").startsWith("α")) {
                elem.setAttribute("as", String.format("α%d", index));
                index += 1;
            }
            wrapper.appendChild(arg);
        }
    }
}
