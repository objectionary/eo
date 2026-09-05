/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * One XMIR fragment, read into a reduction tree.
 *
 * <p>Dispatches become sites, the data carriers become literals, by
 * {@link Carrier}, and a
 * {@code ξ} reference to a declared void becomes the symbol of that
 * void, named positionally so that no spelling of a void name ever
 * leaks into a marker, and a {@code ξ.ρ} reference to the formation
 * being lowered, with arguments, becomes the {@link Again} of its own
 * body. A {@code ξ} reference to a helper the formation binds next to
 * its body becomes the helper's own body, read in place: the helper is
 * an application over the same voids, so it stands wherever it is
 * named, twice when named twice, and identical sites collapse into one
 * step anyway. A helper that reads itself, directly or through another
 * helper, is a cycle and is refused. The parser rolls a dispatch chain
 * rooted in a reference into the base itself, so {@code ξ.b.size.plus}
 * unrolls here into nested sites, with the arguments of the element
 * attached to the last link. Anything else is refused, since its
 * meaning depends on a context the reduction does not carry.</p>
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
     * The name of the formation being lowered, or empty when the
     * fragment is not the body of one.
     */
    private final String self;

    /**
     * The helpers the formation binds next to its body: names to their
     * {@code <o/>} elements.
     */
    private final Map<String, Xnav> helpers;

    /**
     * The helpers being read at the moment, outermost first, so that a
     * helper reading itself is caught.
     */
    private final Collection<String> trail;

    /**
     * Ctor.
     * @param xmir The XMIR fragment to read, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     */
    public Parsed(final Xnav xmir, final Map<String, String> inputs) {
        this(xmir, inputs, "");
    }

    /**
     * Ctor.
     * @param xmir The XMIR fragment to read, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     * @param name The name of the formation the fragment is the body of,
     *  whose calls to itself through {@code ξ.ρ} become repeats
     */
    public Parsed(final Xnav xmir, final Map<String, String> inputs, final String name) {
        this(xmir, inputs, name, Collections.emptyMap());
    }

    /**
     * Ctor.
     * @param xmir The XMIR fragment to read, an {@code <o/>} element
     * @param inputs The voids of the fragment: names to formas, in order
     * @param name The name of the formation the fragment is the body of,
     *  whose calls to itself through {@code ξ.ρ} become repeats
     * @param bound The helpers the formation binds next to its body:
     *  names to their {@code <o/>} elements, read in place when named
     */
    public Parsed(final Xnav xmir, final Map<String, String> inputs,
        final String name, final Map<String, Xnav> bound) {
        this(xmir, inputs, name, bound, Collections.emptyList());
    }

    private Parsed(final Xnav xmir, final Map<String, String> inputs,
        final String name, final Map<String, Xnav> bound, final Collection<String> above) {
        this.fragment = xmir;
        this.voids = inputs;
        this.self = name;
        this.helpers = bound;
        this.trail = above;
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
        if (base.startsWith("Φ.")) {
            out = new Carrier(node).literal();
        } else if (this.recursive(base)) {
            out = new Again(
                this.bound(Parsed.kids(node)).stream()
                    .map(Binding::value)
                    .collect(Collectors.toList())
            );
        } else if (base.startsWith("ξ.")) {
            out = this.chained(node, base.substring(2));
        } else if (base.length() > 1 && base.charAt(0) == '.') {
            out = this.dispatched(node, base);
        } else {
            throw new IllegalStateException(
                String.format("The base '%s' cannot stand in a lowered fragment", base)
            );
        }
        return out;
    }

    private boolean recursive(final String base) {
        return !this.self.isEmpty() && base.equals(String.format("ξ.ρ.%s", this.self));
    }

    private Term referenced(final String name) {
        final List<String> names = new ArrayList<>(this.voids.keySet());
        final int idx = names.indexOf(name);
        final Term out;
        if (idx >= 0) {
            out = new Symbol(String.format("v%d", idx), this.voids.get(name));
        } else if (this.helpers.containsKey(name)) {
            out = this.expanded(name);
        } else {
            throw new IllegalStateException(
                String.format(
                    "The reference 'ξ.%s' names no void or helper of the fragment", name
                )
            );
        }
        return out;
    }

    private Term expanded(final String name) {
        if (this.trail.contains(name)) {
            throw new IllegalStateException(
                String.format(
                    "The helper 'ξ.%s' reads itself, so the fragment never settles", name
                )
            );
        }
        final Collection<String> deeper = new LinkedHashSet<>(this.trail);
        deeper.add(name);
        return new Parsed(
            this.helpers.get(name), this.voids, this.self, this.helpers, deeper
        ).term();
    }

    private Term chained(final Xnav node, final String path) {
        final String[] parts = path.split("\\.", -1);
        Term out = this.referenced(parts[0]);
        final int last = parts.length - 1;
        if (last == 0 && !Parsed.kids(node).isEmpty()) {
            throw new IllegalStateException(
                String.format("The reference 'ξ.%s' cannot take arguments", path)
            );
        }
        for (int idx = 1; idx < last; ++idx) {
            out = new Site(parts[idx], out, new ArrayList<>(0));
        }
        if (last > 0) {
            out = new Site(parts[last], out, this.bound(Parsed.kids(node)));
        }
        return out;
    }

    private Term dispatched(final Xnav node, final String base) {
        final List<Xnav> kids = Parsed.kids(node);
        if (kids.isEmpty()) {
            throw new IllegalStateException(
                String.format("The dispatch '%s' has no receiver", base)
            );
        }
        return new Site(
            base.substring(1),
            this.parsed(kids.get(0)),
            this.bound(kids.subList(1, kids.size()))
        );
    }

    private List<Binding> bound(final List<Xnav> nodes) {
        final List<Binding> args = new ArrayList<>(nodes.size());
        for (final Xnav kid : nodes) {
            final String name = kid.attribute("as").text().orElse("");
            if (name.isEmpty()) {
                throw new IllegalStateException(
                    "An argument without a binding name cannot be reduced"
                );
            }
            args.add(new Binding(name, this.parsed(kid)));
        }
        return args;
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
