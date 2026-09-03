/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * One XMIR fragment, read into a reduction tree.
 *
 * <p>Dispatches become sites, literal carriers become literals, and a
 * {@code ξ} reference to a declared void becomes the symbol of that
 * void, named positionally so that no spelling of a void name ever
 * leaks into a marker. Anything else is refused, since its meaning
 * depends on a context the reduction does not carry — the same contract
 * {@link Expression} keeps for the constant folding path.</p>
 *
 * @since 0.76.0
 */
public final class Parsed {

    /**
     * The XMIR fragment to read, an {@code <o/>} element.
     */
    private final Xnav fragment;

    /**
     * The voids of the fragment: names to formas, in declaration order.
     */
    private final Map<String, String> voids;

    /**
     * Ctor.
     * @param xmir The XMIR fragment to read, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public Parsed(final Xnav xmir, final Map<String, String> inputs) {
        this.fragment = xmir;
        this.voids = inputs;
    }

    /**
     * The tree of the fragment.
     * @return The root term
     */
    public Term term() {
        return this.parsed(this.fragment);
    }

    private Term parsed(final Xnav node) {
        final String base = node.attribute("base").text().orElse("");
        final Term out;
        if ("Φ.true".equals(base)) {
            out = new Literal("bool", "01-");
        } else if ("Φ.false".equals(base)) {
            out = new Literal("bool", "00-");
        } else if ("Φ.number".equals(base)) {
            out = new Literal("number", Parsed.datum(Parsed.kids(node), base));
        } else if ("Φ.bytes".equals(base)) {
            out = new Literal("bytes", Parsed.datum(Parsed.kids(node), base));
        } else if (base.startsWith("ξ.")) {
            out = this.referenced(base.substring(2));
        } else if (base.length() > 1 && base.charAt(0) == '.') {
            out = this.dispatched(node, base);
        } else {
            throw new IllegalStateException(
                String.format("The base '%s' cannot stand in a lowered fragment", base)
            );
        }
        return out;
    }

    private Term referenced(final String name) {
        final List<String> names = new ArrayList<>(this.voids.keySet());
        final int idx = names.indexOf(name);
        if (idx < 0) {
            throw new IllegalStateException(
                String.format("The reference 'ξ.%s' names no void of the fragment", name)
            );
        }
        return new Symbol(String.format("v%d", idx), this.voids.get(name));
    }

    private Term dispatched(final Xnav node, final String base) {
        final List<Xnav> kids = Parsed.kids(node);
        if (kids.isEmpty()) {
            throw new IllegalStateException(
                String.format("The dispatch '%s' has no receiver", base)
            );
        }
        final List<Binding> args = new ArrayList<>(kids.size() - 1);
        for (final Xnav kid : kids.subList(1, kids.size())) {
            final String name = kid.attribute("as").text().orElse("");
            if (name.isEmpty()) {
                throw new IllegalStateException(
                    "An argument without a binding name cannot be reduced"
                );
            }
            args.add(new Binding(name, this.parsed(kid)));
        }
        return new Site(base.substring(1), this.parsed(kids.get(0)), args);
    }

    private static String datum(final List<Xnav> kids, final String base) {
        List<Xnav> inner = kids;
        if ("Φ.number".equals(base)) {
            if (inner.size() != 1
                || !"Φ.bytes".equals(inner.get(0).attribute("base").text().orElse(""))) {
                throw new IllegalStateException(
                    "A number literal must wrap exactly one bytes carrier"
                );
            }
            inner = Parsed.kids(inner.get(0));
        }
        if (inner.size() != 1
            || inner.get(0).attribute("base").text().isPresent()
            || !Parsed.kids(inner.get(0)).isEmpty()) {
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
