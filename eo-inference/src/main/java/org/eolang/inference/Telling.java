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
 * to the T it says, and every unknown it gives is left out. A copy keeps
 * its own name when its {@code @} gives no name, or gives D or a hidden
 * name, and a copy whose {@code @} gives only T is named T. A formation
 * and its voids are told too, from the copies of it that count: what each
 * void holds, what Java gives, and how many of those copies are copies a
 * stop keeps. Whatever the engine cannot name is an
 * unknown of one of four kinds: {@code ?unknown-empty}, a void nobody fills
 * in a copy a call made, said only on that void's own row, since a read of
 * it is T; {@code ?unknown-study}, a void nobody fills in a copy that stands
 * for a caller who is not there; {@code ?unknown-java}, a void only Java
 * fills; and {@code ?unknown-loop}, a read nothing ever answers. A
 * {@code ^} no object went into, of a formation with no name of its own
 * whose call made no copy, is told as a void nobody fills. Two rows are
 * exceptions to the naming of a decorator copy: the row of a {@code ^},
 * and the row of the object a name is read off, for each copy the name is
 * an attribute of, a void or a bound attribute of its formation. Such a
 * row names the formation of the copy, and a {@code reduced} next to the
 * names says what those copies reduce to, when that is something other
 * than the names (#9655). A name a copy lacks is read off what its
 * {@code @} gives, so the row names what that {@code @} gives, as anywhere
 * else. A bound void that gives no label at all once the walk is
 * over is a defect of the walk, and so is an unknown of no kind, and
 * telling stops on either.</p>
 *
 * @since 0.76.0
 */
final class Telling {

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
        final Map<String, Map<String, Integer>> holders = new HashMap<>(0);
        for (final Map.Entry<Key, Ids> entry : answers.entrySet()) {
            Telling.count(
                sets, entry.getKey().place(),
                String.join("\t", this.labels(entry.getKey(), entry.getValue(), holders))
            );
        }
        final Map<String, Map<String, Integer>> groups = new LinkedHashMap<>(0);
        final Map<String, Integer> study = new HashMap<>(0);
        for (final int copy : copies) {
            this.group(copy, groups, holders);
            if (this.state.kept(copy)) {
                study.merge(this.world.value(copy).form(), 1, Integer::sum);
            }
        }
        for (final Map.Entry<String, Map<String, Integer>> entry : groups.entrySet()) {
            sets.putIfAbsent(entry.getKey(), entry.getValue());
        }
        final Map<String, Map<String, Integer>> reduction = this.reduction();
        reduction.putAll(holders);
        return new Told(sets, reduction, this.state.lands(), study);
    }

    private boolean counted(final int ctx) {
        return this.state.known(ctx) && this.world.value(ctx).kind() == 'O'
            && this.state.counts(ctx);
    }

    private Set<String> labels(
        final Key key, final Ids answer, final Map<String, Map<String, Integer>> holders
    ) {
        final Ints got = this.state.untidy(key, answer);
        final Set<String> labels = new TreeSet<>(new CodeOrder());
        if (got.isEmpty()) {
            labels.add(this.missing(key));
        } else if (this.world.formation(key.place())) {
            for (int pos = 0; pos < got.size(); ++pos) {
                labels.add(this.word(got.get(pos)));
            }
        } else {
            final String name = this.off(key.place());
            final Set<String> reduced = new TreeSet<>(new CodeOrder());
            for (int pos = 0; pos < got.size(); ++pos) {
                final Set<String> gives = this.last(got.get(pos));
                reduced.addAll(gives);
                if (this.owns(got.get(pos), name)) {
                    labels.add(this.word(got.get(pos)));
                } else {
                    labels.addAll(gives);
                }
            }
            if (!reduced.equals(labels)) {
                Telling.count(holders, key.place(), String.join(" ", reduced));
            }
        }
        return labels;
    }

    private String off(final String place) {
        final String site = place.substring(0, Math.max(place.lastIndexOf('.'), 0));
        final String name;
        if (this.world.reads(site) && this.world.read(site).of().equals(place)) {
            name = this.world.read(site).name();
        } else {
            name = "";
        }
        return name;
    }

    private boolean owns(final int num, final String name) {
        final Value value = this.world.value(this.world.bare(num));
        return !name.isEmpty() && value.kind() == 'O' && this.world.form(value.form()).has(name);
    }

    private String missing(final Key key) {
        final String label;
        if (EndStep.NOBODY.equals(this.state.token(key.place(), key.value()))) {
            label = "?unknown-java";
        } else {
            label = "?unknown-loop";
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

    private void group(
        final int copy, final Map<String, Map<String, Integer>> groups,
        final Map<String, Map<String, Integer>> holders
    ) {
        final String form = this.world.value(copy).form();
        final Formation shape = this.world.form(form);
        Telling.count(groups, form, form);
        for (final String name : shape.order()) {
            Telling.count(
                groups, String.join(".", form, name), String.join("\t", this.said(copy, name))
            );
        }
        if (shape.open("ρ")) {
            final String place = String.join(".", form, "ρ");
            final Set<String> names = this.rho(copy);
            Telling.count(groups, place, String.join("\t", names));
            final Set<String> reduced = this.lowered(copy);
            if (!reduced.isEmpty() && !reduced.equals(names)) {
                Telling.count(holders, place, String.join(" ", reduced));
            }
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

    private Set<String> said(final int copy, final String name) {
        final Value value = this.world.value(copy);
        final Formation shape = this.world.form(value.form());
        final String holds = shape.holds(name);
        final Set<String> said = new TreeSet<>(new CodeOrder());
        if (Values.JBORN.equals(value.birth()) || (value.bound() & shape.bit(name)) == 0) {
            said.add(this.vacant(copy, holds));
        } else {
            for (final int val : this.state.untidy(copy, name).array()) {
                said.addAll(this.last(val));
            }
            if (!holds.isEmpty()) {
                said.add(holds);
            }
            if (said.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "The void %s of %s is bound, but nothing in it gives a label",
                        name, this.world.full(copy)
                    )
                );
            }
        }
        return said;
    }

    private Set<String> rho(final int copy) {
        final Set<String> said = new TreeSet<>(new CodeOrder());
        final Ints row = this.caret(copy);
        if (row.isEmpty()) {
            said.add(this.vacant(copy, ""));
        } else {
            for (final int val : row.array()) {
                said.add(this.word(val));
            }
        }
        return said;
    }

    private Set<String> lowered(final int copy) {
        final Set<String> said = new TreeSet<>(new CodeOrder());
        for (final int val : this.caret(copy).array()) {
            said.addAll(this.last(val));
        }
        return said;
    }

    private Ints caret(final int copy) {
        final Value value = this.world.value(copy);
        final Ints row;
        if (Values.JBORN.equals(value.birth()) || (value.bound() & Formation.RHO) == 0) {
            row = new Ints();
        } else {
            row = this.state.untidy(copy, "ρ");
        }
        return row;
    }

    private Set<String> last(final int num) {
        final int inside = this.world.bare(num);
        if (!this.finals.containsKey(inside)) {
            this.finals.put(inside, this.walked(inside));
        }
        return this.finals.get(inside);
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
        said.removeIf(label -> label.startsWith("?unknown"));
        final Set<String> result = new HashSet<>(1);
        if (said.equals(Set.of("T"))
            || said.stream().anyMatch(Telling::told) && said.stream().allMatch(Telling::known)) {
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
            label = "?unknown-java";
        } else if (inside == Values.GAP) {
            label = "?unknown-study";
        } else if (value.kind() == 'O') {
            label = value.form();
        } else if (value.kind() == 'D' || value.kind() == 'T' || value.kind() == 'Φ') {
            label = String.valueOf(value.kind());
        } else if (value.kind() == 'U') {
            throw new IllegalStateException(
                String.format(
                    "%s is an unknown of none of the four kinds the engine tells",
                    this.world.full(inside)
                )
            );
        } else {
            label = String.format("?%s", value.kind());
        }
        return label;
    }

    private static boolean known(final String label) {
        return Telling.told(label) || label.charAt(0) == '?' || "T".equals(label);
    }

    private static boolean told(final String label) {
        return label.charAt(0) != '?' && Telling.plain(label);
    }

    private static boolean plain(final String label) {
        return !"T".equals(label) && !"D".equals(label) && !label.contains("🌵");
    }

    private String vacant(final int copy, final String holds) {
        final String label;
        if (!holds.isEmpty()) {
            label = holds;
        } else if (Values.JBORN.equals(this.world.value(copy).birth())) {
            label = "?unknown-java";
        } else if (this.state.kept(copy)) {
            label = "?unknown-study";
        } else {
            label = "?unknown-empty";
        }
        return label;
    }

    private static void count(
        final Map<String, Map<String, Integer>> sets, final String place, final String labels
    ) {
        sets.computeIfAbsent(place, key -> new HashMap<>(1)).merge(labels, 1, Integer::sum);
    }
}
