/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The boxes of a build, read out of its documents.
 *
 * <p>It takes every XMIR of the build and the formas of it. It answers one
 * box per named formation that declares arguments and stands under named
 * formations only, since that is the unit of lowering, each remembering
 * what it answers, what its receiver carries and the forma of every
 * argument. Anonymous formations, thunks, atoms and data objects get
 * none.</p>
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
                && !new Carrier(node.getAttribute("loc")).data()) {
                out.add(this.box(node));
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

    private Box box(final Element node) {
        final String place = node.getAttribute("loc");
        final String parent;
        if (Planted.reaches(node)) {
            parent = Planted.receiver(place);
        } else {
            parent = "-";
        }
        String carrier = this.formas.at(place);
        if (carrier.isEmpty()) {
            carrier = "object";
        }
        final Map<String, String> row = new LinkedHashMap<>(0);
        row.put("locator", place);
        row.put("carrier", carrier);
        row.put("parent", parent);
        row.put(
            "voids",
            Planted.voids(node).stream().map(
                name -> String.format(
                    "%s:%s", name, this.typed(String.format("%s.%s", place, name))
                )
            ).collect(Collectors.joining(" "))
        );
        return new Box(row);
    }

    private static String receiver(final String place) {
        String out = new Carrier(place.substring(0, place.lastIndexOf('.'))).forma();
        if (out.isEmpty()) {
            out = "object";
        }
        return out;
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
