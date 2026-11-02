/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * What a mark tells: one entry for each answer of the object under it.
 *
 * <p>A mark colours the word by its {@link Band} and lists what the object
 * answers, with how many of its copies answer it, the most copies first. A
 * name is listed as it is and a mark of the engine in words. Formations with
 * no name of their own are listed by their {@link Shapes}, one entry for all
 * the formations of one shape, counted in the copies that answer any of them,
 * with a link to every line where one of them is written: {@code line 4} or
 * {@code lines 4, 9} in the same file, {@code bytes.eo:12} in another, and
 * the locator as the link's hover text.</p>
 *
 * @since 0.76.0
 */
final class Entries {

    /**
     * What the table says of each place.
     */
    private final Table table;

    /**
     * The formations of the program.
     */
    private final Shapes shapes;

    /**
     * The path of the source file of the page, relative to the program.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param answers What the table says of each place
     * @param formations The formations of the program
     * @param file The path of the source file of the page, relative to the
     *  program
     */
    Entries(final Table answers, final Shapes formations, final String file) {
        this.table = answers;
        this.shapes = formations;
        this.name = file;
    }

    /**
     * Colour a mark and put under it what the object answers.
     *
     * @param bit The mark, a {@code bit} of a page
     * @param loc The locator of the object the mark tells
     */
    void told(final Element bit, final String loc) {
        final Document page = bit.getOwnerDocument();
        bit.setAttribute("band", new Band(this.shapes.folded(loc)).name());
        final Element told = page.createElement("told");
        told.setAttribute("loc", loc);
        told.setAttribute("copies", Integer.toString(this.table.copies(loc)));
        bit.appendChild(told);
        final Map<String, Integer> said = this.table.said(loc);
        final Map<String, List<String>> shaped = new LinkedHashMap<>(0);
        final Map<String, Element> entries = new HashMap<>(said.size());
        final Map<String, Integer> copies = new HashMap<>(said.size());
        for (final Map.Entry<String, Integer> answer : said.entrySet()) {
            if (this.shapes.hidden(answer.getKey(), loc)) {
                shaped.computeIfAbsent(
                    this.shapes.shown(answer.getKey(), false), shape -> new ArrayList<>(1)
                ).add(answer.getKey());
            } else {
                final Label label = new Label(answer.getKey());
                entries.put(
                    answer.getKey(),
                    Entries.entry(page, label.kind(), answer.getValue(), label.words())
                );
                copies.put(answer.getKey(), answer.getValue());
            }
        }
        for (final List<String> members : shaped.values()) {
            final String first = members.stream().min(new CodeOrder()).orElseThrow();
            final int count = this.table.joined(loc, members);
            final Element entry = Entries.entry(
                page, "object", count, this.shapes.shown(members.get(0), true)
            );
            this.sites(entry, members);
            entries.put(first, entry);
            copies.put(first, count);
        }
        final List<String> order = new ArrayList<>(entries.keySet());
        order.sort(
            Comparator.comparingInt((String key) -> -copies.get(key)).thenComparing(new CodeOrder())
        );
        for (final String key : order) {
            told.appendChild(entries.get(key));
        }
    }

    private static Element entry(
        final Document page, final String kind, final int count, final String words
    ) {
        final Element said = page.createElement("said");
        said.setAttribute("kind", kind);
        said.setAttribute("n", Integer.toString(count));
        if (!words.isEmpty()) {
            said.appendChild(page.createTextNode(words));
        }
        return said;
    }

    private void sites(final Element entry, final List<String> members) {
        final List<String> sorted = new ArrayList<>(members);
        sorted.sort(
            Comparator.comparing((String label) -> !this.shapes.file(label).equals(this.name))
                .thenComparing(this.shapes::file, new CodeOrder())
                .thenComparingInt(this::number)
        );
        int start = 0;
        while (start < sorted.size()) {
            final String file = this.shapes.file(sorted.get(start));
            int end = start;
            while (end < sorted.size() && this.shapes.file(sorted.get(end)).equals(file)) {
                end = end + 1;
            }
            for (int index = start; index < end; index = index + 1) {
                final String label = sorted.get(index);
                final String line = this.shapes.line(label);
                final Element site = entry.getOwnerDocument().createElement("at");
                if (index == start) {
                    site.setAttribute("lead", this.lead(file, end - start));
                } else {
                    site.setAttribute("lead", "");
                }
                site.setAttribute("href", this.href(file).concat(line));
                site.setAttribute("loc", label);
                if (!line.isEmpty()) {
                    site.appendChild(entry.getOwnerDocument().createTextNode(line));
                }
                entry.appendChild(site);
            }
            start = end;
        }
    }

    private String lead(final String file, final int count) {
        final String found;
        if (file.equals(this.name) && count == 1) {
            found = "line ";
        } else if (file.equals(this.name)) {
            found = "lines ";
        } else {
            found = file.concat(":");
        }
        return found;
    }

    private String href(final String file) {
        final String found;
        if (file.equals(this.name)) {
            found = "#L";
        } else {
            found = "../".repeat(this.depth()).concat(file).concat(".html#L");
        }
        return found;
    }

    private int depth() {
        return (int) this.name.chars().filter(glyph -> glyph == '/').count();
    }

    private int number(final String label) {
        final String line = this.shapes.line(label);
        final int found;
        if (line.isEmpty()) {
            found = 0;
        } else {
            found = Integer.parseInt(line);
        }
        return found;
    }
}
