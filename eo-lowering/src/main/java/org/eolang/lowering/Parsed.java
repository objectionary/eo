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
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * One XMIR fragment, read into a reduction tree.
 *
 * <p>Dispatches become sites, the data carriers become literals, by
 * {@link Carrier}, an {@code as-bytes} dispatch and a {@code dataized}
 * object alike become the {@link Forced} bytes of what they are applied
 * to, since a const is the latter followed by the former and forcing is
 * already what a protocol does, and a
 * {@code ξ} reference to a declared void becomes the symbol of that
 * void, named positionally so that no spelling of a void name ever
 * leaks into a marker, and a {@code ξ.ρ} reference to the formation
 * being lowered, with arguments, becomes the {@link Again} of its own
 * body. A {@code ξ} reference to a helper the formation binds next to
 * its body becomes the helper's own body, read in place: an application
 * over the same voids stands wherever it is named, and a formation of
 * its own is applied where it is named, its voids bound to the argument
 * terms in a {@link Scope} of its own, the way phino would bind them,
 * so its body stands there with every void spelled out. A helper named
 * twice stands twice, and identical sites collapse into one step
 * anyway. A helper that reads itself, directly or through another
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
     * What the names of the fragment mean.
     */
    private final Scope scope;

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
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public Parsed(final Xnav xmir, final Map<String, String> inputs,
        final String name, final Map<String, Xnav> bound) {
        this(xmir, new Scope(inputs, name, bound), Collections.emptyList());
    }

    /**
     * Ctor.
     * @param xmir The XMIR fragment to read, an {@code <o/>} element
     * @param where What the names of the fragment mean
     * @param above The helpers being read at the moment, outermost first
     */
    Parsed(final Xnav xmir, final Scope where, final Collection<String> above) {
        this.fragment = xmir;
        this.scope = where;
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
        if ("Φ.dataized".equals(base)) {
            out = new Forced(this.parsed(Parsed.target(node)));
        } else if (base.startsWith("Φ.")) {
            out = new Carrier(node).literal();
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

    private Term chained(final Xnav node, final String path) {
        final String[] parts = path.split("\\.", -1);
        final List<Binding> args = this.bound(Parsed.kids(node));
        Scope where = this.scope;
        int hops = 0;
        while (hops < parts.length && "ρ".equals(parts[hops]) && !where.root()) {
            where = where.above();
            ++hops;
        }
        if (hops == parts.length) {
            throw new IllegalStateException(
                String.format("The reference 'ξ.%s' names a formation, not a value", path)
            );
        }
        final int last = parts.length - 1;
        Term out;
        int next = hops + 1;
        if ("ρ".equals(parts[hops])) {
            out = Parsed.again(where, path, parts, args);
            next = parts.length;
        } else if (hops == last) {
            out = new Reference(where, this.trail, parts[hops], args).term();
        } else {
            out = new Reference(where, this.trail, parts[hops], new ArrayList<>(0)).term();
        }
        for (int idx = next; idx < last; ++idx) {
            out = Parsed.site(parts[idx], out, new ArrayList<>(0));
        }
        if (next <= last) {
            out = Parsed.site(parts[last], out, args);
        }
        return out;
    }

    private static Term again(final Scope where, final String path,
        final String[] parts, final List<Binding> args) {
        if (where.name().isEmpty() || parts.length != 2 || !parts[1].equals(where.name())) {
            throw new IllegalStateException(
                String.format(
                    "The reference 'ξ.%s' reaches through ρ beyond the formation being lowered",
                    path
                )
            );
        }
        return new Again(args.stream().map(Binding::value).collect(Collectors.toList()));
    }

    private Term dispatched(final Xnav node, final String base) {
        final List<Xnav> kids = Parsed.kids(node);
        if (kids.isEmpty()) {
            throw new IllegalStateException(
                String.format("The dispatch '%s' has no receiver", base)
            );
        }
        return Parsed.site(
            base.substring(1),
            this.parsed(kids.get(0)),
            this.bound(kids.subList(1, kids.size()))
        );
    }

    private static Term site(final String method, final Term receiver,
        final List<Binding> args) {
        final Term out;
        if ("as-bytes".equals(method) && args.isEmpty()) {
            out = new Forced(receiver);
        } else {
            out = new Site(method, receiver, args);
        }
        return out;
    }

    private static Xnav target(final Xnav node) {
        final List<Xnav> kids = Parsed.kids(node);
        if (kids.size() != 1) {
            throw new IllegalStateException(
                String.format(
                    "The dataized object must force exactly one target, not %d", kids.size()
                )
            );
        }
        return kids.get(0);
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
