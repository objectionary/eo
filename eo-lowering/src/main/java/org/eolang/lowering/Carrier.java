/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.List;
import java.util.stream.Collectors;

/**
 * One data carrier of an XMIR fragment, read into the literal it carries.
 *
 * <p>A number and a string wrap exactly one bytes carrier, which wraps
 * exactly one plain datum; bytes wrap the datum directly; the two bools
 * carry nothing, since each is its own datum. Any element based on
 * {@code Φ} that is shaped otherwise is refused, since it is no datum
 * and its meaning depends on a context the reduction does not
 * carry.</p>
 *
 * @since 0.76.0
 */
public final class Carrier {

    /**
     * The XMIR element to read, an {@code <o/>} based on {@code Φ}.
     */
    private final Xnav node;

    /**
     * Ctor.
     * @param xmir The XMIR element to read, an {@code <o/>} based on {@code Φ}
     */
    public Carrier(final Xnav xmir) {
        this.node = xmir;
    }

    /**
     * The literal the element carries.
     * @return The literal, with its forma and its bytes
     */
    public Term literal() {
        final String base = this.node.attribute("base").text().orElse("");
        final Term out;
        if ("Φ.true".equals(base)) {
            out = new Literal("bool", "FF-");
        } else if ("Φ.false".equals(base)) {
            out = new Literal("bool", "00-");
        } else if ("Φ.number".equals(base) || "Φ.string".equals(base)) {
            out = new Literal(base.substring(2), this.datum(base));
        } else if ("Φ.bytes".equals(base)) {
            out = new Literal("bytes", this.datum(base));
        } else {
            throw new IllegalStateException(
                String.format(
                    "The base '%s' carries no datum, so it cannot stand in a lowered fragment",
                    base
                )
            );
        }
        return out;
    }

    private String datum(final String base) {
        List<Xnav> inner = Carrier.kids(this.node);
        if (!"Φ.bytes".equals(base)) {
            if (inner.size() != 1
                || !"Φ.bytes".equals(inner.get(0).attribute("base").text().orElse(""))) {
                throw new IllegalStateException(
                    String.format(
                        "The literal '%s' must wrap exactly one bytes carrier", base
                    )
                );
            }
            inner = Carrier.kids(inner.get(0));
        }
        if (inner.size() != 1
            || inner.get(0).attribute("base").text().isPresent()
            || !Carrier.kids(inner.get(0)).isEmpty()) {
            throw new IllegalStateException(
                String.format("The carrier '%s' does not wrap a plain datum", base)
            );
        }
        return inner.get(0).text().orElse("").replaceAll("\\s+", "");
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
