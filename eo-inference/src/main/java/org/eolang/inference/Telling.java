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
     * The world the walk goes over.
     */
    private final World world;

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
     * @param world The world the walk goes over
     */
    Telling(final State walk, final World world) {
        this.state = walk;
        this.world = world;
        this.finals = new LinkedHashMap<>(0);
        this.walking = new BitSet();
    }

    /**
     * Tell every place.
     *
     * @return What is told
     */
    Told told() {
        final Map<Key, Ids> answers = new LinkedHashMap<>(0);
        for (final Key key : this.state.questions()) {
            if (key.kind() == 'e' && this.state.counts(key.value())) {
                answers.put(key, this.state.stored(key));
            }
        }
        final List<Integer> copies = new ArrayList<>(0);
        for (int ctx = 0; ctx < this.world.size(); ++ctx) {
            if (this.counted(ctx)) {
                copies.add(ctx);
            }
        }
        final Map<String, Map<String, Integer>> sets = new HashMap<>(0);
        for (final Map.Entry<Key, Ids> entry : answers.entrySet()) {
            Telling.count(
                sets, entry.getKey().place(),
                String.join("\t", this.labels(entry.getKey(), entry.getValue()))
            );
        }
        final Map<String, Map<String, Integer>> groups = new LinkedHashMap<>(0);
        for (final int copy : copies) {
            this.group(copy, groups);
        }
        for (final Map.Entry<String, Map<String, Integer>> entry : groups.entrySet()) {
            sets.putIfAbsent(entry.getKey(), entry.getValue());
        }
        return new Told(sets, this.reduction(), this.state.lands());
    }

    private boolean counted(final int ctx) {
        return this.state.known(ctx) && this.world.value(ctx).kind() == 'O'
            && this.state.counts(ctx);
    }

    private Set<String> labels(final Key key, final Ids answer) {
        final Ints got = this.state.untidy(key, answer);
        final Set<String> labels = new TreeSet<>(new CodeOrder());
        if (got.isEmpty()) {
            labels.add(this.missing(key));
        } else if (this.world.formation(key.place())) {
            for (int pos = 0; pos < got.size(); ++pos) {
                labels.add(this.word(got.get(pos)));
            }
        } else {
            for (int pos = 0; pos < got.size(); ++pos) {
                labels.addAll(this.last(got.get(pos)));
            }
        }
        return labels;
    }

    private String missing(final Key key) {
        final String label;
        if (EndStep.NOBODY.equals(this.state.token(key.place(), key.value()))) {
            label = "?open-void-atom";
        } else {
            label = "?can't-tell";
        }
        return label;
    }

    private Map<String, Map<String, Integer>> reduction() {
        final Map<String, Map<String, Integer>> reduction = new HashMap<>(0);
        for (final Map.Entry<Integer, Set<String>> entry : this.finals.entrySet()) {
            final Value value = this.world.value(entry.getKey());
            if (value.kind() == 'O') {
                final Set<String> sorted = new TreeSet<>(new CodeOrder());
                sorted.addAll(entry.getValue());
                Telling.count(reduction, value.form(), String.join(" ", sorted));
            }
        }
        return reduction;
    }

    private void group(final int copy, final Map<String, Map<String, Integer>> groups) {
        final String form = this.world.value(copy).form();
        final Formation shape = this.world.form(form);
        Telling.count(groups, form, form);
        for (final String name : shape.order()) {
            Telling.count(
                groups, String.join(".", form, name), String.join("\t", this.said(copy, name))
            );
        }
        if (shape.open("ρ")) {
            Telling.count(groups, String.join(".", form, "ρ"), String.join("\t", this.rho(copy)));
        }
        if (!shape.returns().isEmpty()) {
            Telling.count(
                groups, String.join(".", form, "λ"), String.join("\t", Telling.gives(shape))
            );
        }
    }

    private Set<String> said(final int copy, final String name) {
        final Value value = this.world.value(copy);
        final Formation shape = this.world.form(value.form());
        final String holds = shape.holds(name);
        final Set<String> said = new TreeSet<>(new CodeOrder());
        if (Values.JBORN.equals(value.birth()) || (value.bound() & shape.bit(name)) == 0) {
            said.add(Telling.vacant(value, holds));
        } else {
            for (final int val : this.state.untidy(copy, name).array()) {
                said.addAll(this.last(val));
            }
            if (!holds.isEmpty()) {
                said.add(holds);
            }
            if (said.isEmpty()) {
                said.add("?can't-tell");
            }
        }
        return said;
    }

    private Set<String> rho(final int copy) {
        final Value value = this.world.value(copy);
        final Set<String> said = new TreeSet<>(new CodeOrder());
        if (this.state.handedToJava(copy)) {
            said.add(Telling.ONLY_JAVA);
        } else if (Values.JBORN.equals(value.birth()) || (value.bound() & Formation.RHO) == 0) {
            said.add(Telling.vacant(value, ""));
        } else {
            for (final int val : this.state.untidy(copy, "ρ").array()) {
                said.addAll(this.last(val));
            }
            if (said.isEmpty()) {
                said.add("?can't-tell");
            }
        }
        return said;
    }

    private Set<String> last(final int num) {
        final int inside = this.world.bare(num);
        Set<String> got = this.finals.get(inside);
        if (got == null) {
            got = this.walked(inside);
            this.finals.put(inside, got);
        }
        return got;
    }

    private Set<String> walked(final int num) {
        final Value value = this.world.value(num);
        final Set<String> result;
        if (value.kind() == 'O' && this.state.canon(value.form())) {
            result = new HashSet<>(1);
            result.add(value.form());
        } else if (value.kind() == 'O') {
            result = this.named(num);
        } else {
            result = new HashSet<>(1);
            result.add(this.word(num));
        }
        return result;
    }

    private Set<String> named(final int num) {
        final String form = this.world.value(num).form();
        final Formation shape = this.world.form(form);
        final Set<String> result = new HashSet<>(1);
        if (!this.world.decorator(form) && !shape.returns().isEmpty()
            && !shape.has("φ") && this.world.formation(shape.returns())) {
            result.add(shape.returns());
        } else {
            final List<String> voids = this.behind(num);
            if (voids.isEmpty() || this.walking.get(num)) {
                result.add(form);
            } else {
                result.addAll(this.followed(num, voids));
            }
        }
        return result;
    }

    private List<String> behind(final int num) {
        final String form = this.world.value(num).form();
        final Formation shape = this.world.form(form);
        final List<String> voids;
        if (this.world.decorator(form) && this.state.counts(num)) {
            voids = List.of("φ");
        } else if (this.world.decorator(form)
            || shape.returns().isEmpty() || shape.has("φ")) {
            voids = List.of();
        } else {
            voids = this.world.own(form);
        }
        return voids;
    }

    private Set<String> followed(final int num, final List<String> voids) {
        final Value value = this.world.value(num);
        this.walking.set(num);
        final Set<String> said = new HashSet<>(1);
        try {
            for (final String name : voids) {
                final Ids got = this.state.attr(
                    num, name, new Site(String.join(".", value.form(), name), value.tag())
                );
                for (int idx = 0; idx < got.size(); ++idx) {
                    said.addAll(this.last(got.get(idx)));
                }
            }
        } finally {
            this.walking.clear(num);
        }
        said.removeIf(label -> label.startsWith("?open-void"));
        final Set<String> result = new HashSet<>(1);
        if (said.equals(Set.of("T")) || Telling.clean(said)) {
            result.addAll(said);
        } else {
            result.add(value.form());
        }
        return result;
    }

    private String word(final int num) {
        final int inside = this.world.bare(num);
        final Value value = this.world.value(inside);
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

    private static boolean clean(final Set<String> said) {
        return said.stream().anyMatch(Telling::told) && said.stream().allMatch(Telling::known);
    }

    private static boolean known(final String label) {
        return Telling.told(label) || label.charAt(0) == '?' || "T".equals(label);
    }

    private static boolean told(final String label) {
        return label.charAt(0) != '?' && label.charAt(0) != '!' && Telling.plain(label);
    }

    private static boolean plain(final String label) {
        return !"T".equals(label) && !"D".equals(label) && !label.contains("🌵");
    }

    private static String vacant(final Value value, final String holds) {
        final String label;
        if (holds.isEmpty() && Values.JBORN.equals(value.birth())) {
            label = "?open-void-atom";
        } else if (holds.isEmpty()) {
            label = "?open-void";
        } else {
            label = holds;
        }
        return label;
    }

    private static Set<String> gives(final Formation shape) {
        final Set<String> gives = new TreeSet<>(new CodeOrder());
        if (shape.gives().isEmpty()) {
            gives.add(shape.returns());
        } else {
            gives.addAll(shape.gives());
        }
        return gives;
    }

    private static void count(
        final Map<String, Map<String, Integer>> sets, final String place, final String labels
    ) {
        sets.computeIfAbsent(place, key -> new HashMap<>(1)).merge(labels, 1, Integer::sum);
    }
}
