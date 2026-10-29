/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * How a place is named (design doc 5.2), over every copy that counts.
 *
 * <p>The answer at a place is the union of its answers over all its copies
 * that count, and each copy says a set of labels there. A copy is named by
 * its formation; a copy that counts, of a decorator or of an atom that
 * returns its own voids, is named by every name its {@code @} gives, next
 * to the "can't tell" and T it says. A copy keeps its own name when its
 * {@code @} gives no name, or gives a failing read, D or a hidden name, and
 * a copy whose {@code @} gives only T is named T. A formation and its voids
 * are told too, from the copies of it that count: what each void holds,
 * and what Java gives.</p>
 *
 * @since 0.76.0
 */
final class Telling {

    /**
     * The mark of what only Java puts into a {@code ρ}.
     */
    static final String ONLY_JAVA = "!only-java-fills-ρ";

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * What each value is named, once worked out.
     */
    private final Map<Integer, Set<String>> finals;

    /**
     * The values being named now.
     */
    private final BitSet walking;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Telling(final State walk) {
        this.state = walk;
        this.finals = new LinkedHashMap<>(0);
        this.walking = new BitSet();
    }

    /**
     * Tell every place.
     *
     * @return What is told
     * @checkstyle CyclomaticComplexityCheck (100 lines)
     * @checkstyle ExecutableStatementCountCheck (100 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    Told told() {
        final List<Key> answered = new ArrayList<>(0);
        final List<Ids> answers = new ArrayList<>(0);
        for (final Key key : this.state.memo.questions()) {
            if (key.kind() == 'e' && this.state.counting.counts(key.value())) {
                answered.add(key);
                answers.add(this.state.memo.stored(key));
            }
        }
        final List<Integer> copies = new ArrayList<>(0);
        final BitSet known = this.state.passes.known();
        for (int ctx = known.nextSetBit(0); ctx >= 0; ctx = known.nextSetBit(ctx + 1)) {
            if (this.state.values.get(ctx).kind() == 'O' && this.state.counting.counts(ctx)) {
                copies.add(ctx);
            }
        }
        final Map<String, Map<String, Integer>> sets = new HashMap<>(0);
        for (int idx = 0; idx < answered.size(); ++idx) {
            final Key key = answered.get(idx);
            final Ints got = this.state.end.untidy(key, answers.get(idx));
            final Set<String> labels = new TreeSet<>(new CodeOrder());
            if (got.isEmpty()) {
                final String mark = this.state.end.mark(key.place(), key.value());
                if (EndStep.NOBODY.equals(mark)) {
                    labels.add("?open-void-atom");
                } else {
                    labels.add("?can't-tell");
                }
            } else if (this.state.program.formation(key.place())) {
                for (int pos = 0; pos < got.size(); ++pos) {
                    labels.add(this.word(got.get(pos)));
                }
            } else {
                for (int pos = 0; pos < got.size(); ++pos) {
                    labels.addAll(this.last(got.get(pos)));
                }
            }
            Telling.count(sets, key.place(), String.join("\t", labels));
        }
        final Map<String, Map<String, Integer>> groups = new LinkedHashMap<>(0);
        for (final int copy : copies) {
            this.group(copy, groups);
        }
        for (final Map.Entry<String, Map<String, Integer>> entry : groups.entrySet()) {
            sets.putIfAbsent(entry.getKey(), entry.getValue());
        }
        final Map<String, Map<String, Integer>> reduction = new HashMap<>(0);
        for (final Map.Entry<Integer, Set<String>> entry : this.finals.entrySet()) {
            final Value value = this.state.values.get(entry.getKey());
            if (value.kind() == 'O') {
                final Set<String> sorted = new TreeSet<>(new CodeOrder());
                sorted.addAll(entry.getValue());
                Telling.count(reduction, value.form(), String.join(" ", sorted));
            }
        }
        return new Told(sets, reduction, this.state.calls.lands());
    }

    /**
     * Tell a formation and its voids from one copy of it that counts.
     *
     * @param copy The copy
     * @param groups Where the sets go, by place
     * @checkstyle CyclomaticComplexityCheck (60 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    private void group(final int copy, final Map<String, Map<String, Integer>> groups) {
        final Value value = this.state.values.get(copy);
        final String form = value.form();
        final Formation shape = this.state.program.form(form);
        final boolean java = Values.JBORN.equals(value.birth());
        final String hole;
        if (java) {
            hole = "?open-void-atom";
        } else {
            hole = "?open-void";
        }
        Telling.count(groups, form, form);
        for (final String name : shape.order()) {
            final String holds = shape.holds(name);
            final Set<String> said = new TreeSet<>(new CodeOrder());
            if (java || (value.bound() & shape.bit(name)) == 0) {
                if (holds.isEmpty()) {
                    said.add(hole);
                } else {
                    said.add(holds);
                }
            } else {
                final Ints row = this.state.end.untidy(copy, name);
                for (final int val : row.array()) {
                    said.addAll(this.last(val));
                }
                if (!holds.isEmpty()) {
                    said.add(holds);
                }
                if (said.isEmpty()) {
                    said.add("?can't-tell");
                }
            }
            Telling.count(groups, String.join(".", form, name), String.join("\t", said));
        }
        if (shape.open("ρ")) {
            final Set<String> said = new TreeSet<>(new CodeOrder());
            if (this.state.callbacks.handedToJava(copy)) {
                said.add(Telling.ONLY_JAVA);
            } else if (java || (value.bound() & Formation.RHO) == 0) {
                said.add(hole);
            } else {
                final Ints row = this.state.end.untidy(copy, "ρ");
                for (final int val : row.array()) {
                    said.addAll(this.last(val));
                }
                if (said.isEmpty()) {
                    said.add("?can't-tell");
                }
            }
            Telling.count(groups, String.join(".", form, "ρ"), String.join("\t", said));
        }
        if (!shape.returns().isEmpty()) {
            final Set<String> gives = new TreeSet<>(new CodeOrder());
            if (shape.gives().isEmpty()) {
                gives.add(shape.returns());
            } else {
                gives.addAll(shape.gives());
            }
            Telling.count(groups, String.join(".", form, "λ"), String.join("\t", gives));
        }
    }

    /**
     * What a value is named, at the end of its chain of {@code @}.
     *
     * @param num The value
     * @return The labels
     */
    private Set<String> last(final int num) {
        final int inside = this.state.values.bare(num);
        Set<String> got = this.finals.get(inside);
        if (got == null) {
            got = this.walked(inside);
            this.finals.put(inside, got);
        }
        return got;
    }

    /**
     * What a value is named, worked out now.
     *
     * @param num The value, with no wrapper
     * @return The labels
     * @checkstyle CyclomaticComplexityCheck (80 lines)
     * @checkstyle NPathComplexityCheck (80 lines)
     */
    @SuppressWarnings({"PMD.CognitiveComplexity", "PMD.NPathComplexity"})
    private Set<String> walked(final int num) {
        final Value value = this.state.values.get(num);
        final Set<String> result = new HashSet<>(1);
        if (value.kind() != 'O') {
            result.add(this.word(num));
        } else if (this.state.copies.canon(value.form())) {
            result.add(value.form());
        } else {
            final String form = value.form();
            final Formation shape = this.state.program.form(form);
            List<String> voids = List.of();
            if (this.state.roles.decorator(form)) {
                if (this.state.counting.counts(num)) {
                    voids = List.of("φ");
                }
            } else if (!shape.returns().isEmpty() && !shape.has("φ")) {
                if (this.state.program.formation(shape.returns())) {
                    result.add(shape.returns());
                } else {
                    voids = this.state.roles.own(form);
                }
            }
            if (result.isEmpty() && (voids.isEmpty() || this.walking.get(num))) {
                result.add(form);
            }
            if (result.isEmpty()) {
                this.walking.set(num);
                final Set<String> said = new HashSet<>(1);
                try {
                    for (final String name : voids) {
                        final Ids got = this.state.reads.attr(
                            num, name, String.join(".", form, name), value.tag()
                        );
                        for (int idx = 0; idx < got.size(); ++idx) {
                            said.addAll(this.last(got.get(idx)));
                        }
                    }
                } finally {
                    this.walking.clear(num);
                }
                said.removeIf(label -> label.startsWith("?open-void"));
                if (said.equals(Set.of("T"))) {
                    result.addAll(said);
                } else if (said.stream().anyMatch(Telling::told) && said.stream().allMatch(
                    label -> Telling.told(label) || label.charAt(0) == '?' || "T".equals(label)
                )) {
                    result.addAll(said);
                } else {
                    result.add(form);
                }
            }
        }
        return result;
    }

    /**
     * The label of a value as it is.
     *
     * @param num The value
     * @return The label
     */
    private String word(final int num) {
        final int inside = this.state.values.bare(num);
        final Value value = this.state.values.get(inside);
        final String label;
        if (inside == Values.JGAP) {
            label = "?open-void-atom";
        } else if (value.kind() == 'O') {
            label = value.form();
        } else if (value.kind() == 'D' || value.kind() == 'T' || value.kind() == 'Φ') {
            label = String.valueOf(value.kind());
        } else if (value.kind() == 'X') {
            label = String.format("!%s", value.form());
        } else if (value.kind() == 'U') {
            label = String.format("?%s", value.form().replace(' ', '-'));
        } else {
            label = String.format("?%s", value.kind());
        }
        return label;
    }

    /**
     * Whether a label is a name the answers may tell: not a mark, not T or
     * D, and not hidden.
     *
     * @param label The label
     * @return TRUE if it is
     */
    private static boolean told(final String label) {
        return label.charAt(0) != '?' && label.charAt(0) != '!'
            && !"T".equals(label) && !"D".equals(label) && !label.contains("🌵");
    }

    /**
     * Count one more copy that says a set of labels at a place.
     *
     * @param sets The sets, by place
     * @param place The place
     * @param labels The labels, joined
     */
    private static void count(
        final Map<String, Map<String, Integer>> sets, final String place, final String labels
    ) {
        sets.computeIfAbsent(place, key -> new HashMap<>(1)).merge(labels, 1, Integer::sum);
    }
}
