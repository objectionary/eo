/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Rewrite the pure formations of one XMIR into synthetic atoms.
 *
 * <p>A formation qualifies when it is a named direct attribute of a
 * top-level object, its body is voids plus one {@code φ} plus helpers
 * nothing outside can name, and every void is witnessed in the tables
 * of {@code eo:inference} as a number, a string, bytes or a bool, so a
 * symbolic carrier can stand for it, or as a tuple, which the atom holds
 * as the object itself and asks its length and its elements of by
 * dispatching back into EO. A helper is an attribute the source
 * privatized with {@code >>}, or a const the parser wrapped, and it
 * shows up under a synthetic {@code a🌵} name: the body reads it in
 * place, applying it to its arguments when it is a formation of its
 * own, so it is folded into the atom and leaves with the body. A
 * public attribute keeps the formation as written, since deleting the
 * body must not change the object's interface, and a helper that stays
 * reachable would be dispatchable with nothing behind it. The formation
 * may declare {@code ρ}, and its body may reach through it only to call
 * the formation itself again, which the reduction turns into a repeat;
 * a helper reaches through {@code ρ} to the voids and the other helpers
 * of the formation, which the atom carries, and helpers that apply one
 * another in tail positions become one loop with them; any other use of
 * {@code ρ} depends on a context the atom does not
 * carry and refuses. Purity needs no separate analysis:
 * the reduction itself is constructive proof, since it settles only a
 * body made of literals, void references, and the lowerable operations,
 * and refuses everything else. Such a formation is reduced into a
 * protocol, the protocol is rendered into a Java body, the body goes
 * into a sidecar file named by its own digest, and the formation keeps
 * only its voids, the digest, and a {@code λ} marker — the shape
 * {@code lowered.xsl} later renders into an atom class, which binds
 * {@code ρ} of its own, so a declared {@code ρ} leaves with the body.
 * Whatever refuses
 * along the way — an unwitnessed void, an operation outside the tables,
 * a body that needs no computation — leaves the formation as
 * written.</p>
 *
 * @since 0.76.0
 */
public final class Lowered implements Rewrite {

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * The tables with the witnessed forma of each void.
     */
    private final Formas formas;

    /**
     * The directory for the sidecar bodies.
     */
    private final Path atoms;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param tables The tables with the witnessed forma of each void
     * @param home The directory for the sidecar bodies
     */
    public Lowered(final Phino exe, final Formas tables, final Path home) {
        this.phino = exe;
        this.formas = tables;
        this.atoms = home;
    }

    @Override
    public int rewrite(final Xnav doc) throws IOException {
        int count = 0;
        if (!this.formas.blank()) {
            for (final Xnav node : Lowered.candidates(doc)) {
                final String place = node.attribute("loc").text().orElse("");
                if (!place.isEmpty() && this.lowered(node, place)) {
                    ++count;
                }
            }
        }
        return count;
    }

    private boolean lowered(final Xnav node, final String place) throws IOException {
        final Map<String, String> inputs = this.voids(node, place);
        final List<Xnav> bodies = Lowered.bodies(node);
        final List<Xnav> kids = Lowered.kids(node);
        final long rhos = kids.stream().filter(Lowered::rho).count();
        final long hidden = kids.stream().filter(Lowered::hidden).count();
        boolean done = false;
        if (!inputs.isEmpty() && bodies.size() == 1
            && kids.size() == inputs.size() + 1 + (int) rhos + (int) hidden) {
            done = this.spliced(node, bodies.get(0), inputs);
        }
        return done;
    }

    private boolean spliced(final Xnav node, final Xnav body,
        final Map<String, String> inputs) throws IOException {
        String text = "";
        String carrier = "";
        try {
            final Program program = new Reduction(
                this.phino, body, inputs, 8,
                node.attribute("name").text().orElse(""),
                Lowered.helpers(node)
            ).program();
            if (program.bodies().size() > 1
                || !program.bodies().get(0).protocol().moves().isEmpty()) {
                text = new JavaAtom(program).text();
                carrier = program.carrier();
            }
        } catch (final IllegalStateException | IOException ex) {
            carrier = "";
        }
        final boolean done = !carrier.isEmpty();
        if (done) {
            Lowered.marked(
                (Element) node.node(), body, inputs,
                new Sidecar(this.atoms, text).save(), carrier
            );
        }
        return done;
    }

    private static void marked(final Element element, final Xnav body,
        final Map<String, String> inputs, final String digest, final String carrier) {
        element.setAttribute("pure", "true");
        element.setAttribute("lowered", digest);
        element.removeChild(body.node());
        for (final Xnav kid : Lowered.kids(new Xnav(element))) {
            if (Lowered.rho(kid) || Lowered.hidden(kid)) {
                element.removeChild(kid.node());
            }
        }
        final Document doc = element.getOwnerDocument();
        final Element marker = doc.createElement("o");
        marker.setAttribute("name", "λ");
        marker.setAttribute("atom", String.format("Φ.%s", carrier));
        element.appendChild(marker);
        for (final Xnav kid : Lowered.kids(new Xnav(element))) {
            final String name = kid.attribute("name").text().orElse("");
            if (inputs.containsKey(name)) {
                ((Element) kid.node()).setAttribute(
                    "type", String.format("Φ.%s", inputs.get(name))
                );
            }
        }
    }

    private Map<String, String> voids(final Xnav node, final String place) {
        final Map<String, String> out = new LinkedHashMap<>();
        for (final Xnav kid : Lowered.kids(node)) {
            if (!"∅".equals(kid.attribute("base").text().orElse(""))) {
                continue;
            }
            final String name = kid.attribute("name").text().orElse("");
            if ("ρ".equals(name)) {
                continue;
            }
            final String forma = this.formas.given(String.format("%s.%s", place, name));
            if (forma.isEmpty()) {
                out.clear();
                break;
            }
            out.put(name, forma);
        }
        return out;
    }

    private static boolean rho(final Xnav kid) {
        return "∅".equals(kid.attribute("base").text().orElse(""))
            && "ρ".equals(kid.attribute("name").text().orElse(""));
    }

    private static boolean hidden(final Xnav kid) {
        return kid.attribute("name").text().orElse("").startsWith("a🌵");
    }

    private static Map<String, Xnav> helpers(final Xnav node) {
        final Map<String, Xnav> out = new LinkedHashMap<>();
        for (final Xnav kid : Lowered.kids(node)) {
            if (Lowered.hidden(kid)) {
                out.put(kid.attribute("name").text().get(), kid);
            }
        }
        return out;
    }

    private static Collection<Xnav> candidates(final Xnav doc) {
        final Collection<Xnav> out = new ArrayList<>(0);
        for (final Xnav top : Lowered.kids(doc.element("object"))) {
            if (top.attribute("base").text().isPresent()) {
                continue;
            }
            for (final Xnav kid : Lowered.kids(top)) {
                final String name = kid.attribute("name").text().orElse("");
                if (kid.attribute("base").text().isEmpty()
                    && !name.isEmpty() && !"λ".equals(name)) {
                    out.add(kid);
                }
            }
        }
        return out;
    }

    private static List<Xnav> bodies(final Xnav node) {
        return Lowered.kids(node).stream()
            .filter(kid -> "φ".equals(kid.attribute("name").text().orElse("")))
            .filter(kid -> !kid.attribute("base").text().orElse("").isEmpty())
            .filter(kid -> !"∅".equals(kid.attribute("base").text().orElse("")))
            .collect(Collectors.toList());
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
