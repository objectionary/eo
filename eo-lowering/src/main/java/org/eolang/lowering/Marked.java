/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Every marker phino left in a fragment turned into what it stands for: a
 * reference, when the symbol reads a void as written, or a call of an
 * atom, whose body is the program of the symbol rendered into Java and
 * saved as a sidecar.
 *
 * <p>The atom is a formation named {@code l🌵<digest>} appended to the
 * fragment, with one void per input, and the marker, together with the
 * carrier wrapping around it, becomes a call of that formation with the
 * inputs as arguments, each a reference from where the marker stood: up
 * one {@code ρ} per formation between the marker and the fragment, and
 * then along the path of the void. A formation entered where it stands
 * is reached from the nearest formation the fragment and it are both
 * inside, or by its full locator when that formation is outside the
 * top-level object, which is a copy the same everywhere. When the marker
 * is the whole {@code φ} of a nested fragment that binds nothing but its
 * voids and the handles of its consts, and the marker reads nothing but its
 * own voids, the fragment itself becomes the atom instead, since a
 * top-level one is a class of its own already.</p>
 *
 * @since 0.77.0
 */
final class Marked {

    /**
     * The symbols.
     */
    private static final Pattern SYMBOL = Pattern.compile("S\\d+");

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
        for (final Map.Entry<Element, String> site : this.sites().entrySet()) {
            final String ref = this.table.reference(site.getValue());
            if (ref.isEmpty()) {
                this.lowered(site.getKey(), site.getValue());
                ++made;
            } else {
                final Element out = Marked.alike(site.getKey());
                out.setAttribute(
                    "base", Marked.climbed(this.depth(site.getKey()), ref)
                );
                site.getKey().getParentNode().replaceChild(out, site.getKey());
            }
        }
        return made;
    }

    private Map<Element, String> sites() {
        final Map<Element, String> out = new LinkedHashMap<>(0);
        for (final Element lambda : Marked.lambdas(this.fragment, new ArrayList<>(0))) {
            String sym = lambda.getTextContent();
            Element site = (Element) lambda.getParentNode();
            Element parent = (Element) site.getParentNode();
            if (Marked.wraps(parent, "Φ.bytes")) {
                site = parent;
                parent = (Element) site.getParentNode();
                if (Marked.wraps(parent, "Φ.number") || Marked.wraps(parent, "Φ.string")) {
                    site = parent;
                    parent = (Element) site.getParentNode();
                }
            } else if ("guard".equals(site.getAttribute("name"))
                && Marked.wraps(parent.getParentNode(), "Φ.bool")) {
                site = (Element) parent.getParentNode();
                parent = (Element) site.getParentNode();
            }
            final String owner = this.table.receiver(sym);
            if ("Φ.tuple".equals(parent.getAttribute("base")) && !owner.isEmpty()) {
                site = parent;
                sym = owner;
            }
            out.put(site, sym);
        }
        return out;
    }

    private void lowered(final Element site, final String sym) throws IOException {
        final Map<String, String> inputs = this.table.inputs(sym);
        final boolean whole = site.getParentNode().equals(this.fragment)
            && !"object".equals(this.fragment.getParentNode().getNodeName())
            && "φ".equals(site.getAttribute("name"))
            && inputs.keySet().stream()
            .allMatch(key -> key.indexOf('.') < 0 && !key.startsWith("box:"))
            && Marked.bare(this.fragment, site);
        final Map<String, String> names = new LinkedHashMap<>(inputs.size());
        for (final String key : inputs.keySet()) {
            if (whole) {
                names.put(key, key);
            } else {
                names.put(key, String.format("v%d", names.size()));
            }
        }
        final Program program = this.table.program(sym, names);
        final String digest = new Sidecar(this.atoms, new JavaAtom(program).text()).save();
        final boolean pure = inputs.values().stream().allMatch(Marked.DATA::contains)
            && program.bodies().stream().noneMatch(body -> Marked.enters(body.protocol()));
        if (whole) {
            this.fragment.removeChild(site);
            for (final Element kid : new Kids(this.fragment)) {
                final String name = kid.getAttribute("name");
                if ("ρ".equals(name) && "∅".equals(kid.getAttribute("base"))
                    || name.startsWith("a🌵")) {
                    this.fragment.removeChild(kid);
                } else if (inputs.containsKey(name) && Marked.DATA.contains(inputs.get(name))) {
                    kid.setAttribute("type", String.format("Φ.%s", inputs.get(name)));
                }
            }
            Marked.stamped(this.fragment, digest, program.carrier(), pure);
        } else {
            final String atom = String.format("l🌵%s", digest);
            if (new Kids(this.fragment).all().stream()
                .noneMatch(kid -> atom.equals(kid.getAttribute("name")))) {
                this.fragment.appendChild(
                    this.sibling(atom, digest, inputs, names, program.carrier(), pure)
                );
            }
            final int depth = this.depth(site);
            final Element call = Marked.alike(site);
            call.setAttribute("base", Marked.climbed(depth, atom));
            int idx = 0;
            for (final String key : inputs.keySet()) {
                final Element arg = this.fragment.getOwnerDocument().createElement("o");
                arg.setAttribute("as", String.format("α%d", idx));
                arg.setAttribute("base", this.bound(key, depth));
                call.appendChild(arg);
                ++idx;
            }
            site.getParentNode().replaceChild(call, site);
        }
    }

    private Element sibling(final String name, final String digest,
        final Map<String, String> inputs, final Map<String, String> names,
        final String carrier, final boolean pure) {
        final Element atom = this.fragment.getOwnerDocument().createElement("o");
        atom.setAttribute("name", name);
        atom.setAttribute(
            "loc", String.format("%s.%s", this.fragment.getAttribute("loc"), name)
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
        Marked.stamped(atom, digest, carrier, pure);
        return atom;
    }

    private String bound(final String key, final int depth) {
        final String out;
        if (key.startsWith("box:")) {
            final String target = key.substring(4);
            final String place = this.fragment.getAttribute("loc");
            final String common = Marked.common(
                place, target.substring(0, target.lastIndexOf('.'))
            );
            final String top = this.top();
            if (common.equals(top) || common.startsWith(String.format("%s.", top))) {
                out = Marked.climbed(
                    depth + place.split("\\.").length - common.split("\\.").length,
                    target.substring(common.length() + 1)
                );
            } else {
                out = target;
            }
        } else {
            out = Marked.climbed(depth, key);
        }
        return out;
    }

    private String top() {
        Element cursor = this.fragment;
        while (!"object".equals(cursor.getParentNode().getNodeName())) {
            cursor = (Element) cursor.getParentNode();
        }
        return cursor.getAttribute("loc");
    }

    private int depth(final Element site) {
        int out = 0;
        Node cursor = site.getParentNode();
        while (!cursor.equals(this.fragment)) {
            if (!((Element) cursor).hasAttribute("base")) {
                ++out;
            }
            cursor = cursor.getParentNode();
        }
        return out;
    }

    private static boolean bare(final Element fragment, final Element site) {
        return new Kids(fragment).all().stream().allMatch(
            kid -> kid.equals(site) || "∅".equals(kid.getAttribute("base"))
                || kid.getAttribute("name").startsWith("a🌵")
        );
    }

    private static void stamped(final Element formation, final String digest,
        final String carrier, final boolean pure) {
        formation.setAttribute("lowered", digest);
        if (pure) {
            formation.setAttribute("pure", "true");
        }
        final Element lambda = formation.getOwnerDocument().createElement("o");
        lambda.setAttribute("name", "λ");
        lambda.setAttribute("atom", String.format("Φ.%s", carrier));
        formation.appendChild(lambda);
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

    private static String climbed(final int depth, final String path) {
        return String.format(
            "ξ%s.%s", String.join("", Collections.nCopies(depth, ".ρ")), path
        );
    }

    private static String common(final String left, final String right) {
        final String[] one = left.split("\\.");
        final String[] two = right.split("\\.");
        final List<String> out = new ArrayList<>(one.length);
        for (int idx = 0; idx < Math.min(one.length, two.length); ++idx) {
            if (!one[idx].equals(two[idx])) {
                break;
            }
            out.add(one[idx]);
        }
        return String.join(".", out);
    }

    private static boolean wraps(final Node node, final String base) {
        return node.getNodeType() == Node.ELEMENT_NODE
            && base.equals(((Element) node).getAttribute("base"))
            && new Kids(node).all().size() == 1;
    }

    private static List<Element> lambdas(final Element node, final List<Element> out) {
        for (final Element kid : new Kids(node)) {
            if ("λ".equals(kid.getAttribute("name"))
                && Marked.SYMBOL.matcher(kid.getTextContent()).matches()) {
                out.add(kid);
            } else {
                Marked.lambdas(kid, out);
            }
        }
        return out;
    }
}
