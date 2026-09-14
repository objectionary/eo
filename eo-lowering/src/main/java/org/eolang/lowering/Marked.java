/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.w3c.dom.Element;

/**
 * Every marker phino left in a fragment turned into what it stands for.
 *
 * <p>It takes the fragment, the symbol table behind the markers and the
 * directory the sidecars live in. It rewrites the fragment in place — each
 * marker becomes a reference to a void, or a call of a new atom whose Java
 * body is saved as a sidecar — and answers how many markers it turned.</p>
 *
 * <p>A fragment absorbed whole keeps the ρ it declares when its program
 * reads it, since that void is the receiver its callers dispatch onto: an
 * object that drops it answers every {@code x.f y} with a refusal to take
 * a receiver it no longer declares.</p>
 *
 * @since 0.77.0
 */
final class Marked {

    /**
     * The formas Java carries as data.
     */
    private static final List<String> DATA = Arrays.asList("number", "bool", "bytes", "string");

    /**
     * The fragment.
     */
    private final Element fragment;

    /**
     * The table of the run.
     */
    private final Table table;

    /**
     * Where the sidecars go.
     */
    private final Path atoms;

    /**
     * Ctor.
     *
     * @param formation The fragment
     * @param symbols The table of the run
     * @param home Where the sidecars go
     */
    Marked(final Element formation, final Table symbols, final Path home) {
        this.fragment = formation;
        this.table = symbols;
        this.atoms = home;
    }

    /**
     * Turn the markers.
     *
     * @return How many atoms were made
     * @throws IOException If a sidecar cannot be saved
     */
    int apply() throws IOException {
        int made = 0;
        for (final Map.Entry<Element, String> site
            : new Sites(this.fragment, this.table).all().entrySet()) {
            final String ref = this.table.reference(site.getValue());
            if (ref.isEmpty()) {
                ++made;
                if (this.lowered(site.getKey(), site.getValue())) {
                    break;
                }
            } else {
                final Element out = Marked.alike(site.getKey());
                out.setAttribute("base", new Route(this.fragment, site.getKey()).to(ref));
                site.getKey().getParentNode().replaceChild(out, site.getKey());
            }
        }
        return made;
    }

    private boolean lowered(final Element site, final String sym) throws IOException {
        final Map<String, String> inputs = this.table.inputs(sym);
        final boolean whole = this.whole(site, inputs);
        final Map<String, String> names = new LinkedHashMap<>(inputs.size());
        for (final String key : inputs.keySet()) {
            if (whole) {
                names.put(key, key);
            } else {
                names.put(key, String.format("v%d", names.size()));
            }
        }
        final Program program = this.table.program(sym, names);
        final Stamp stamp = new Stamp(
            new Sidecar(this.atoms, new JavaAtom(program).text()).save(),
            program.carrier(),
            inputs.values().stream().allMatch(Marked.DATA::contains)
                && program.bodies().stream().noneMatch(body -> Marked.enters(body.protocol()))
        );
        if (whole) {
            this.absorbed(site, inputs, stamp);
        } else {
            this.called(site, inputs, names, stamp);
        }
        return whole;
    }

    private boolean whole(final Element site, final Map<String, String> inputs) {
        return this.nested(site)
            && inputs.keySet().stream().allMatch(
                key -> key.indexOf('.') < 0 && !key.startsWith("box:")
            )
            && Marked.bare(this.fragment, site);
    }

    private boolean nested(final Element site) {
        return site.getParentNode().equals(this.fragment)
            && !"object".equals(this.fragment.getParentNode().getNodeName())
            && "φ".equals(site.getAttribute("name"));
    }

    private void absorbed(final Element site, final Map<String, String> inputs,
        final Stamp stamp) {
        this.fragment.removeChild(site);
        for (final Element kid : new Kids(this.fragment)) {
            final String name = kid.getAttribute("name");
            if ("ρ".equals(name) && "∅".equals(kid.getAttribute("base"))
                && !inputs.containsKey(name)
                || name.startsWith("a🌵")) {
                this.fragment.removeChild(kid);
            } else if (inputs.containsKey(name) && Marked.DATA.contains(inputs.get(name))) {
                kid.setAttribute("type", String.format("Φ.%s", inputs.get(name)));
            }
        }
        stamp.on(this.fragment);
    }

    private void called(final Element site, final Map<String, String> inputs,
        final Map<String, String> names, final Stamp stamp) {
        if (new Kids(this.fragment).all().stream()
            .noneMatch(kid -> stamp.name().equals(kid.getAttribute("name")))) {
            this.fragment.appendChild(this.sibling(inputs, names, stamp));
        }
        final Route route = new Route(this.fragment, site);
        final Element call = Marked.alike(site);
        call.setAttribute("base", route.to(stamp.name()));
        int idx = 0;
        for (final String key : inputs.keySet()) {
            final Element arg = this.fragment.getOwnerDocument().createElement("o");
            arg.setAttribute("as", String.format("α%d", idx));
            arg.setAttribute("base", route.to(key));
            call.appendChild(arg);
            ++idx;
        }
        site.getParentNode().replaceChild(call, site);
    }

    private Element sibling(final Map<String, String> inputs, final Map<String, String> names,
        final Stamp stamp) {
        final Element atom = this.fragment.getOwnerDocument().createElement("o");
        atom.setAttribute("name", stamp.name());
        atom.setAttribute(
            "loc", String.format("%s.%s", this.fragment.getAttribute("loc"), stamp.name())
        );
        for (final Map.Entry<String, String> vain : names.entrySet()) {
            final Element hole = this.fragment.getOwnerDocument().createElement("o");
            hole.setAttribute("base", "∅");
            hole.setAttribute("name", vain.getValue());
            hole.setAttribute(
                "loc", String.format("%s.%s", atom.getAttribute("loc"), vain.getValue())
            );
            if (Marked.DATA.contains(inputs.get(vain.getKey()))) {
                hole.setAttribute("type", String.format("Φ.%s", inputs.get(vain.getKey())));
            }
            atom.appendChild(hole);
        }
        stamp.on(atom);
        return atom;
    }

    private static boolean bare(final Element fragment, final Element site) {
        return new Kids(fragment).all().stream().allMatch(
            kid -> kid.equals(site) || "∅".equals(kid.getAttribute("base"))
                || kid.getAttribute("name").startsWith("a🌵")
        );
    }

    private static boolean enters(final Protocol proto) {
        return proto.moves().stream().anyMatch(
            step -> step.atom().startsWith("Φ.")
                || step.branches().stream().anyMatch(Marked::enters)
        );
    }

    private static Element alike(final Element site) {
        final Element out = site.getOwnerDocument().createElement("o");
        for (final String attr : new String[] {"name", "as", "loc", "line", "pos", "local"}) {
            if (site.hasAttribute(attr)) {
                out.setAttribute(attr, site.getAttribute(attr));
            }
        }
        return out;
    }
}
