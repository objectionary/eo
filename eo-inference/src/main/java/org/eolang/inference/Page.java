/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * One source file, ready to be shown to somebody.
 *
 * <p>The parser keeps the whole text of a file in the XMIR, beside the objects
 * it read out of it, so a page can show the program as its author wrote it
 * rather than as a machine printed it back. Every object carries the line and
 * column it was written at, and the table carries what its copies answer, and
 * this puts the two together: the source, line by line, with a mark on every
 * stretch of text the table says something about.</p>
 *
 * <p>The bytes of a literal get no mark of their own: there is nothing to find
 * out about {@code 42} beyond that it is what it is (#8538).</p>
 *
 * <p>The tally at the top is counted here too, from the same answers as the
 * marks, over every object with a locator, whether the source spells it or
 * not. A page that says nine tenths green and then draws half the file red
 * would be worse than no page, and counting in one place is what stops
 * that.</p>
 *
 * @since 0.70.0
 */
final class Page {

    /**
     * Where one line of the listing ends and the next begins.
     *
     * <p>Only the breaks the parser counts, and not {@code \R}, which also
     * matches {@code U+2028} and its kin. The parser lets those stand inside
     * a string literal and counts no line for them, so a page that broke on
     * one would put every mark below the literal on the wrong line
     * (#9270).</p>
     */
    private static final Pattern BREAK = Pattern.compile("\\r?\\n");

    /**
     * The XMIR of the file, as {@code pre-inference} left it.
     */
    private final Xnav xmir;

    /**
     * The path of the source file, relative to the program.
     */
    private final String name;

    /**
     * What the table says of each place.
     */
    private final Table table;

    /**
     * The formations of the program.
     */
    private final Shapes shapes;

    /**
     * Ctor.
     *
     * @param file The XMIR of the file, as {@code pre-inference} left it
     * @param path The path of the source file, relative to the program
     * @param answers What the table says of each place
     * @param formations The formations of the program
     */
    Page(final Xnav file, final String path, final Table answers, final Shapes formations) {
        this.xmir = file;
        this.name = path;
        this.table = answers;
        this.shapes = formations;
    }

    /**
     * The page, with a {@code line} per line of the source and the tally of
     * its objects by band.
     *
     * @param page The document the page is made in
     * @return The page
     */
    Element drawn(final Document page) {
        final Element found = page.createElement("page");
        found.setAttribute("file", this.name);
        found.setAttribute(
            "root", "../".repeat((int) this.name.chars().filter(glyph -> glyph == '/').count())
        );
        final Map<String, Integer> counted = this.counted();
        for (final String band : Band.ORDER) {
            found.setAttribute(band, Integer.toString(counted.getOrDefault(band, 0)));
        }
        final List<String> lines = this.lines();
        final Map<Integer, List<Written>> written = this.written();
        final Entries entries = new Entries(this.table, this.shapes, this.name);
        for (int index = 0; index < lines.size(); index = index + 1) {
            final Element row = page.createElement("line");
            row.setAttribute("n", Integer.toString(index + 1));
            new Pieces(
                lines.get(index), written.getOrDefault(index + 1, Collections.emptyList())
            ).drawn(row, entries);
            found.appendChild(row);
        }
        return found;
    }

    private List<String> lines() {
        final List<String> found = new ArrayList<>(0);
        this.root().elements(Filter.withName("listing")).findFirst().ifPresent(
            listing -> found.addAll(
                Arrays.asList(Page.BREAK.split(listing.text().orElse(""), -1))
            )
        );
        if (!found.isEmpty() && found.get(found.size() - 1).isEmpty()) {
            found.remove(found.size() - 1);
        }
        return found;
    }

    private Map<Integer, List<Written>> written() {
        final Map<Integer, List<Written>> found = new HashMap<>(0);
        this.deeper(this.root())
            .filter(this::placed)
            .filter(object -> !this.datum(object))
            .forEach(
                object -> {
                    final Noted noted = new Noted(object);
                    found.computeIfAbsent(
                        Integer.parseInt(noted.says("line")), key -> new ArrayList<>(1)
                    ).add(
                        new Written(
                            noted.says("loc"),
                            Integer.parseInt(noted.says("pos")),
                            noted.says("name"),
                            noted.says("base")
                        )
                    );
                }
            );
        return found;
    }

    private Stream<Xnav> deeper(final Xnav object) {
        return object.elements(Filter.withName("o"))
            .flatMap(kid -> Stream.concat(Stream.of(kid), this.deeper(kid)));
    }

    private boolean placed(final Xnav object) {
        final Noted noted = new Noted(object);
        return !noted.says("loc").isEmpty()
            && !noted.says("line").isEmpty()
            && !noted.says("pos").isEmpty();
    }

    private boolean datum(final Xnav object) {
        return object.elements(Filter.withName("o")).findAny().isEmpty()
            && !object.text().orElse("").strip().isEmpty();
    }

    private Map<String, Integer> counted() {
        final Map<String, Integer> found = new HashMap<>(Band.ORDER.size());
        final NodeList all = this.root().node().getOwnerDocument().getElementsByTagName("o");
        for (int index = 0; index < all.getLength(); index = index + 1) {
            final Node object = all.item(index);
            final String loc = new Noted(new Xnav(object)).says("loc");
            if (!loc.isEmpty()) {
                found.merge(new Band(this.shapes.folded(loc)).name(), 1, Integer::sum);
            }
        }
        return found;
    }

    private Xnav root() {
        return this.xmir.elements().filter(node -> node.node().getNodeType() == Node.ELEMENT_NODE)
            .findFirst()
            .orElseThrow(
                () -> new IllegalStateException(
                    String.format("The XMIR of %s has no root element", this.name)
                )
            );
    }
}
