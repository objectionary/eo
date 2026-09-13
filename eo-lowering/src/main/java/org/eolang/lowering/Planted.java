/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The boxes of a build: one per named formation that declares arguments
 * and stands under named formations only, read out of the XMIR documents.
 *
 * <p>Such a formation is a fragment, the unit of lowering, and its box is
 * the λ the engine serves when another fragment enters it. An anonymous
 * formation is left to phino, which inlines it, and so is one without
 * arguments, a thunk, which folds into whatever enters it. An atom has a
 * λ of its own, which the engine serves through {@code ops.tsv} or phino
 * parks, and a data object such as {@code Φ.number} is the carrier its
 * markers are wrapped into, so neither is boxed. Each box
 * remembers what the formation answers, what its receiver carries when
 * the body reaches for it, and the forma of each of its voids, all read
 * from the tables of the inference.</p>
 *
 * @since 0.77.0
 */
public final class Planted {

    /**
     * The XMIR documents.
     */
    private final List<Path> docs;

    /**
     * The formas of the build.
     */
    private final Formas formas;

    /**
     * Ctor.
     *
     * @param xmirs The XMIR documents
     * @param tables The formas of the build
     */
    public Planted(final List<Path> xmirs, final Formas tables) {
        this.docs = xmirs;
        this.formas = tables;
    }

    /**
     * All the boxes, in document order.
     *
     * @return The boxes
     */
    public List<Box> all() {
        final List<Box> out = new ArrayList<>(0);
        for (final Path doc : this.docs) {
            new Xnav(doc).element("object").elements(Filter.withName("o"))
                .forEach(top -> this.through((Element) top.node(), out));
        }
        return out;
    }

    private void through(final Element node, final List<Box> out) {
        if (!node.hasAttribute("base") && node.hasAttribute("name")
            && !"λ".equals(node.getAttribute("name"))) {
            if (!Planted.voids(node).isEmpty() && !Planted.atom(node)
                && !this.formas.data(node.getAttribute("loc"))) {
                out.add(this.box(node, out.size() + 1));
            }
            for (final Element kid : new Kids(node)) {
                this.through(kid, out);
            }
        }
    }

    private static boolean atom(final Element node) {
        return new Kids(node).all().stream()
            .anyMatch(kid -> "λ".equals(kid.getAttribute("name")));
    }

    private Box box(final Element node, final int index) {
        final String place = node.getAttribute("loc");
        final String parent;
        if (Planted.reaches(node)) {
            parent = this.typed(String.format("%s.ρ", place));
        } else {
            parent = "-";
        }
        String carrier = this.formas.at(place);
        if (carrier.isEmpty()) {
            carrier = "object";
        }
        return new Box(
            Arrays.asList(
                String.format("L_box_%d", index),
                place,
                carrier,
                parent,
                Planted.voids(node).stream()
                    .map(
                        name -> String.format(
                            "%s:%s", name, this.typed(String.format("%s.%s", place, name))
                        )
                    )
                    .collect(Collectors.joining(" "))
            )
        );
    }

    private String typed(final String place) {
        String out = this.formas.known(place);
        if (out.isEmpty()) {
            out = "object";
        }
        return out;
    }

    private static List<String> voids(final Element node) {
        return new Kids(node).all().stream()
            .filter(kid -> "∅".equals(kid.getAttribute("base")))
            .map(kid -> kid.getAttribute("name"))
            .filter(name -> !"ρ".equals(name))
            .collect(Collectors.toList());
    }

    private static boolean reaches(final Element node) {
        boolean out = false;
        for (final Element kid : new Kids(node)) {
            final String base = kid.getAttribute("base");
            if ("∅".equals(base) && "ρ".equals(kid.getAttribute("name"))
                || base.startsWith("ξ.ρ") || Planted.reaches(kid)) {
                out = true;
                break;
            }
        }
        return out;
    }
}
