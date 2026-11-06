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
 * {@code D}, {@code <terminator/>} into {@code T},
 * {@code <unknown kind="study"/>} into {@code ?unknown-study}, the same for
 * each of the four kinds, {@code empty}, {@code study}, {@code java} and
 * {@code loop}. A missing attribute is T, and the {@code ^} of a formation
 * handed to an atom is that atom, so the engine writes no failure, and a
 * {@code <fails>} stops the report. The {@code study} of the row of a
 * formation says how many of its copies are study copies, and a row with
 * none has no {@code study}.</p>
 *
 * @since 0.76.0
 */
final class Table {

    /**
     * The kinds of an unknown.
     */
    private static final Set<String> KINDS = Set.of("empty", "study", "java", "loop");

    /**
     * The document {@code answers.xml}.
     */
    private final Xnav answers;

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
     * The study copies of each formation that has any, read on the first
     * question and kept for the rest.
     */
    private final List<Map<String, Integer>> studies;

    /**
     * Ctor.
     *
     * @param answers The document {@code answers.xml}
     */
    Table(final Xnav answers) {
        this.answers = answers;
        this.read = new ArrayList<>(1);
        this.studies = new ArrayList<>(1);
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

    /**
     * How many copies of the formation are study copies.
     *
     * @param place The locator of the formation
     * @return The study copies, none when its row names none
     */
    int study(final String place) {
        if (this.studies.isEmpty()) {
            this.studies.add(
                this.answers.element("answers").elements(Filter.withName("o"))
                    .map(Noted::new)
                    .filter(row -> !row.says("study").isEmpty()).collect(
                        Collectors.toMap(
                            row -> row.says("loc"), row -> Integer.parseInt(row.says("study"))
                        )
                    )
            );
        }
        return this.studies.get(0).getOrDefault(place, 0);
    }

    private Map<Set<String>, Integer> sets(final String place) {
        if (this.read.isEmpty()) {
            this.read.add(this.rows());
        }
        return this.read.get(0).getOrDefault(place, Collections.emptyMap());
    }

    private Map<String, Map<Set<String>, Integer>> rows() {
        final Map<String, Map<Set<String>, Integer>> found = new HashMap<>(0);
        this.answers.element("answers").elements(Filter.withName("o")).forEach(
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
                        Table.labels(group),
                        Integer.parseInt(new Noted(group).says("copies")),
                        Integer::sum
                    );
                }
            }
        );
        return found;
    }

    private static Set<String> labels(final Xnav group) {
        return group.elements()
            .filter(item -> item.node().getNodeType() == Node.ELEMENT_NODE)
            .filter(item -> !"reduced".equals(item.node().getNodeName()))
            .map(Table::label)
            .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    private static String label(final Xnav item) {
        final Noted noted = new Noted(item);
        final String tag = item.node().getNodeName();
        final String found;
        if ("is".equals(tag)) {
            found = noted.says("loc");
        } else if ("unknown".equals(tag)) {
            found = Table.unknown(noted);
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

    private static String unknown(final Noted noted) {
        final String kind = noted.says("kind");
        if (!Table.KINDS.contains(kind)) {
            throw new IllegalArgumentException(
                String.format(
                    "answers.xml has an unknown of kind '%s', which the report does not know",
                    kind
                )
            );
        }
        return String.format("?unknown-%s", kind);
    }
}
