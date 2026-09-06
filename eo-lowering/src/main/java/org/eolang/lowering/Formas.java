/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.w3c.dom.Node;

/**
 * The data formas of the locators of one build, read from the tables
 * of {@code eo:inference}.
 *
 * <p>The links table binds every locator to what fills it: a literal, a
 * void, or a reference to another locator. Chasing the references from a
 * locator lands on an endpoint, and when that endpoint names a carrier
 * forma, everything the locator may ever hold is data of that forma —
 * the same walk {@code purify.xsl} takes in {@code eo:decided}, answered
 * here with the forma instead of a verdict. A
 * void endpoint answers through the provides table, which witnesses what
 * every filling site actually passes in. The two bool states are one
 * forma between them, so a void the sites fill with both is witnessed as
 * a bool all the same. Anything else — a missing row, a cycle, a raw
 * literal — answers with the empty string, and the caller refuses.</p>
 *
 * @since 0.76.0
 */
public final class Formas {

    /**
     * The forma a symbolic carrier stands for, by the locator naming it.
     */
    private static final Map<String, String> CARRIERS = Formas.carriers();

    /**
     * The target locator of every locator whose single filling is a
     * reference; anything else — a void, a literal, an undecidable
     * row — is absent, and the chase asks the provides table instead.
     */
    private final Map<String, String> links;

    /**
     * The witnessed data formas, by the locator of each void.
     */
    private final Map<String, String> voids;

    /**
     * Ctor.
     * @param tables The directory with the tables of {@code eo:inference},
     *  which does not have to exist
     */
    public Formas(final Path tables) {
        this(
            Formas.chained(tables.resolve("links.xml")),
            Formas.witnessed(tables.resolve("provides.xml"))
        );
    }

    /**
     * Ctor.
     * @param rows The single filling of each locator
     * @param given The witnessed data formas, by the locator of each void
     */
    public Formas(final Map<String, String> rows, final Map<String, String> given) {
        this.links = rows;
        this.voids = given;
    }

    /**
     * Whether the tables are absent or empty.
     * @return TRUE when there is nothing to answer from
     */
    public boolean blank() {
        return this.links.isEmpty() && this.voids.isEmpty();
    }

    /**
     * The witnessed forma of one void.
     * @param place The locator of the void
     * @return The forma, or the empty string when it is not witnessed
     *  as a single data forma
     */
    public String given(final String place) {
        return this.voids.getOrDefault(place, "");
    }

    /**
     * The forma at the end of the reference chase from one locator.
     * @param start The locator to chase from
     * @return The forma, or the empty string when the chase refuses
     */
    public String at(final String start) {
        final Set<String> seen = new HashSet<>();
        String out = "";
        String cursor = start;
        while (seen.add(cursor)) {
            if (Formas.CARRIERS.containsKey(cursor)) {
                out = Formas.CARRIERS.get(cursor);
                break;
            }
            final String next = this.links.getOrDefault(cursor, "");
            if (next.isEmpty()) {
                out = this.given(cursor);
                break;
            }
            cursor = next;
        }
        return out;
    }

    private static Map<String, String> chained(final Path table) {
        final Map<String, String> out = new HashMap<>(0);
        if (Files.exists(table)) {
            new Xnav(table)
                .element("links")
                .elements(Filter.withName("type"))
                .forEach(row -> Formas.linked(row, out));
        }
        return out;
    }

    private static void linked(final Xnav row, final Map<String, String> out) {
        final String place = row.attribute("id").text().orElse("");
        final List<Xnav> kids = row.elements()
            .filter(kid -> kid.node().getNodeType() == Node.ELEMENT_NODE)
            .collect(Collectors.toList());
        if (!place.isEmpty() && kids.size() == 1
            && "ref".equals(kids.get(0).node().getNodeName())) {
            kids.get(0).attribute("loc").text().ifPresent(
                loc -> out.put(place, loc)
            );
        }
    }

    private static Map<String, String> witnessed(final Path table) {
        final Map<String, String> out = new HashMap<>(0);
        if (Files.exists(table)) {
            new Xnav(table)
                .element("provides")
                .elements(Filter.withName("type"))
                .forEach(row -> Formas.provided(row, out));
        }
        return out;
    }

    private static void provided(final Xnav row, final Map<String, String> out) {
        final String place = row.attribute("id").text().orElse("");
        if (!place.isEmpty()) {
            row.elements(Filter.withName("attr"))
                .filter(attr -> "true".equals(attr.attribute("void").text().orElse("")))
                .forEach(attr -> Formas.admitted(attr, place, out));
        }
    }

    private static void admitted(final Xnav attr, final String place,
        final Map<String, String> out) {
        final Set<String> kinds = attr.elements(Filter.withName("witnessed"))
            .flatMap(Formas::refs)
            .map(loc -> Formas.CARRIERS.getOrDefault(loc, ""))
            .collect(Collectors.toSet());
        if (kinds.size() == 1 && !kinds.contains("")) {
            attr.attribute("name").text().ifPresent(
                name -> out.put(
                    String.format("%s.%s", place, name), kinds.iterator().next()
                )
            );
        }
    }

    private static Map<String, String> carriers() {
        final Map<String, String> out = new HashMap<>(5);
        out.put("Φ.number", "number");
        out.put("Φ.string", "string");
        out.put("Φ.bytes", "bytes");
        out.put("Φ.true", "bool");
        out.put("Φ.false", "bool");
        out.put("Φ.tuple", "tuple");
        out.put("Φ.tuple.empty", "tuple");
        return out;
    }

    private static Stream<String> refs(final Xnav node) {
        return Stream.concat(
            node.elements(Filter.withName("ref"))
                .map(ref -> ref.attribute("loc").text().orElse("")),
            node.elements().flatMap(Formas::refs)
        ).filter(loc -> !loc.isEmpty());
    }
}
