/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.w3c.dom.Node;

/**
 * What {@code answers.xml} says of each place, as the report reads it.
 *
 * <p>The engine writes a row per place, with the sets of labels its copies
 * say and how many copies say each set; a row whose copies all say one set
 * holds that set's labels right under it. What a formation reduces to is in
 * the row too, and the report leaves it out. A label is counted in the copies
 * of every set that holds it, so a place where 119 copies say {@code T} and
 * {@code Φ.bool} and 112 copies say {@code Φ.bool} alone says
 * {@code Φ.bool} in 231 copies.</p>
 *
 * <p>Each element is read back into the engine's own label:
 * {@code <is loc="Φ.number"/>} into {@code Φ.number}, {@code <data/>} into
 * {@code D}, {@code <terminator/>} into {@code T}, {@code <unknown/>} into
 * {@code ?can't-tell}, {@code <open/>} into {@code ?open-void} or, when Java
 * fills it, {@code ?open-void-atom}, and {@code <fails name="x"/>} into
 * {@code !x}, or {@code !only-java-fills-ρ} when only Java fills the
 * {@code ^}.</p>
 *
 * @since 0.76.0
 */
final class Table {

    /**
     * The document {@code answers.xml}.
     */
    private final Xnav table;

    /**
     * The rows, read on the first question and kept for the rest.
     *
     * <p>A page asks about every object on it and about every formation
     * those objects answer, so the rows are asked for a few hundred thousand
     * times and read once. The list holds either nothing, before the first
     * question, or the rows: each set of labels of a place, by its locator,
     * with how many copies say it.</p>
     */
    private final List<Map<String, Map<Set<String>, Integer>>> read;

    /**
     * Ctor.
     *
     * @param answers The document {@code answers.xml}
     */
    Table(final Xnav answers) {
        this.table = answers;
        this.read = new ArrayList<>(1);
    }

    /**
     * How many copies of the place the engine walked.
     *
     * @param place The locator of the place
     * @return The copies, none when the table has no row for it
     */
    int copies(final String place) {
        return this.sets(place).values().stream().mapToInt(Integer::intValue).sum();
    }

    /**
     * What the place says, with the copies that say each label.
     *
     * @param place The locator of the place
     * @return The labels, in the order the table first gives them
     */
    Map<String, Integer> said(final String place) {
        final Map<String, Integer> found = new LinkedHashMap<>(0);
        for (final Map.Entry<Set<String>, Integer> set : this.sets(place).entrySet()) {
            for (final String label : set.getKey()) {
                found.merge(label, set.getValue(), Integer::sum);
            }
        }
        return found;
    }

    /**
     * How many copies of the place say at least one of the labels.
     *
     * @param place The locator of the place
     * @param labels The labels
     * @return The copies
     */
    int joined(final String place, final Collection<String> labels) {
        return this.sets(place).entrySet().stream()
            .filter(set -> !Collections.disjoint(set.getKey(), labels))
            .mapToInt(Map.Entry::getValue)
            .sum();
    }

    private Map<Set<String>, Integer> sets(final String place) {
        if (this.read.isEmpty()) {
            this.read.add(this.rows());
        }
        return this.read.get(0).getOrDefault(place, Collections.emptyMap());
    }

    private Map<String, Map<Set<String>, Integer>> rows() {
        final Map<String, Map<Set<String>, Integer>> found = new HashMap<>(0);
        this.table.element("answers").elements(Filter.withName("o")).forEach(
            row -> {
                final Map<Set<String>, Integer> sets = found.computeIfAbsent(
                    new Noted(row).says("loc"), place -> new LinkedHashMap<>(1)
                );
                final List<Xnav> groups = row.elements(Filter.withName("set"))
                    .collect(Collectors.toList());
                if (groups.isEmpty()) {
                    groups.add(row);
                }
                for (final Xnav group : groups) {
                    sets.merge(
                        this.labels(group),
                        Integer.parseInt(new Noted(group).says("copies")),
                        Integer::sum
                    );
                }
            }
        );
        return found;
    }

    private Set<String> labels(final Xnav group) {
        return group.elements()
            .filter(item -> item.node().getNodeType() == Node.ELEMENT_NODE)
            .filter(item -> !"reduced".equals(item.node().getNodeName()))
            .map(this::label)
            .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    private String label(final Xnav item) {
        final Noted noted = new Noted(item);
        final String tag = item.node().getNodeName();
        final String found;
        if ("is".equals(tag)) {
            found = noted.says("loc");
        } else if ("fails".equals(tag)) {
            found = this.failed(noted);
        } else if ("open".equals(tag)) {
            found = this.opened(noted);
        } else if ("unknown".equals(tag)) {
            found = this.unknown(noted);
        } else if ("data".equals(tag)) {
            found = "D";
        } else if ("terminator".equals(tag)) {
            found = "T";
        } else {
            throw new IllegalArgumentException(
                String.format("The element <%s> of answers.xml is no label the report knows", tag)
            );
        }
        return found;
    }

    private String failed(final Noted noted) {
        final String found;
        if ("java".equals(noted.says("by"))) {
            found = "!only-java-fills-ρ";
        } else {
            found = String.format("!%s", noted.says("name"));
        }
        return found;
    }

    private String opened(final Noted noted) {
        final String found;
        if ("java".equals(noted.says("by"))) {
            found = "?open-void-atom";
        } else {
            found = "?open-void";
        }
        return found;
    }

    private String unknown(final Noted noted) {
        final String found;
        if (noted.says("why").isEmpty()) {
            found = "?can't-tell";
        } else {
            found = String.format("?%s", noted.says("why"));
        }
        return found;
    }
}
