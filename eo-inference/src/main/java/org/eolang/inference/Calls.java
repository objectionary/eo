/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A call ({@code apply()}): what its heads give once its arguments go into
 * their empty voids.
 *
 * <p>Each argument is worked out in the copy around and reduced (4.2). A
 * call makes one copy per head and place, and each void it fills gets the
 * whole set of its argument, so calls never mix (rule 3). It makes no copy
 * while an argument it binds has nothing yet, the absence rule (4.1). The
 * rules come in here: 3 for an argument whose every value fails, b for an
 * argument that is only an open void, A for a mark, F for a call on what
 * Java made or on a finished shared kind, and call then narrow (3.5) for
 * an object whose {@code @} is its void. Every argument of an atom is
 * dataized, a void its atom calls back gets its copies made, and a void of
 * what Java makes fills its pool.</p>
 *
 * <p>For each head the call also notes where each argument lands: the
 * void of a copy made in EO code, or no void.</p>
 *
 * @since 0.76.0
 */
final class Calls {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * Where the arguments land, by call place and copy around.
     */
    private final Map<Slot, Set<Landing>> lands;

    /**
     * Equal sets of landings, as one object.
     */
    private final Map<Set<Landing>, Set<Landing>> landings;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Calls(final State walk) {
        this.state = walk;
        this.lands = new LinkedHashMap<>(0);
        this.landings = new HashMap<>(0);
    }

    /**
     * Apply heads to arguments.
     *
     * @param heads What the place gives before the call
     * @param args The places of the arguments, in order
     * @param ctx The copy around
     * @param place The place of the call
     * @return What the call gives
     * @checkstyle CyclomaticComplexityCheck (200 lines)
     * @checkstyle ExecutableStatementCountCheck (200 lines)
     * @checkstyle NestedIfDepthCheck (200 lines)
     * @checkstyle JavaNCSSCheck (200 lines)
     */
    @SuppressWarnings({"PMD.CognitiveComplexity", "PMD.NPathComplexity", "PMD.NcssCount"})
    Ints apply(final Ints heads, final List<String> args, final int ctx, final String place) {
        final String tag = this.state.tags.carried(ctx);
        final List<int[]> raw = new ArrayList<>(args.size());
        for (int arg = 0; arg < args.size(); ++arg) {
            final Ids got = this.state.places.evaluate(args.get(arg), ctx);
            final Ints kept = new Ints();
            for (int idx = 0; idx < got.size(); ++idx) {
                if (this.state.values.get(got.get(idx)).kind() != 'X') {
                    kept.add(got.get(idx));
                }
            }
            if (!got.isEmpty()) {
                if (this.state.rules.taken(new Wish('3', place, ctx, arg))) {
                    kept.add(this.state.values.unknown("can't tell"));
                } else if (kept.isEmpty()) {
                    this.state.rules.wish(new Wish('3', place, ctx, arg));
                }
            }
            raw.add(kept.array());
        }
        final List<int[]> argsets = new ArrayList<>(args.size());
        for (final int[] set : raw) {
            argsets.add(this.state.reduction.reduced(set));
        }
        for (final int[] set : argsets) {
            for (final int val : set) {
                this.state.passes.remember(val);
            }
        }
        final Ints out = new Ints();
        final Set<Landing> landed = new HashSet<>(0);
        for (int pos = 0; pos < heads.size(); ++pos) {
            int head = heads.get(pos);
            Value value = this.state.values.get(head);
            if (value.kind() == 'B') {
                head = value.base();
                value = this.state.values.get(head);
            }
            final char kind = value.kind();
            final List<String> free;
            if (kind == 'O') {
                free = this.state.values.opened(value);
            } else {
                free = Collections.emptyList();
            }
            if (kind == 'O' && !Values.JBORN.equals(value.birth())
                && !this.state.copies.shared(head) && args.size() <= free.size()) {
                for (int arg = 0; arg < args.size(); ++arg) {
                    landed.add(new Landing(arg, free.get(arg)));
                }
            } else if (kind != 'O' && kind != 'T' && kind != 'D' && kind != 'X') {
                for (int arg = 0; arg < args.size(); ++arg) {
                    landed.add(new Landing(arg, ""));
                }
            }
            if (kind == 'A') {
                if (args.size() < value.count()) {
                    out.add(this.state.reduction.mark(value.base(), value.count() - args.size()));
                } else if (args.size() == value.count()) {
                    final Value base = this.state.values.get(value.base());
                    if (base.kind() == 'O' && !this.state.values.opened(base).isEmpty()) {
                        out.add(this.state.reduction.mark(value.base(), 0));
                    } else {
                        out.add(value.base());
                    }
                } else {
                    out.add(Values.TERM);
                }
                continue;
            }
            if (kind == 'O' && (Values.JBORN.equals(value.birth()) || this.state.copies.shared(head))
                && !args.isEmpty()) {
                out.add(Values.TERM);
                continue;
            }
            if (kind == 'O' && args.size() > free.size()) {
                out.add(Values.TERM);
                continue;
            }
            boolean narrowed = false;
            if (kind == 'O' && !this.state.roles.passes(value.form()).isEmpty()) {
                final String passed = this.state.roles.passes(value.form());
                if ((value.bound() & this.state.program.form(value.form()).bit(passed)) != 0) {
                    for (final int held : this.state.rows.cell(head, passed)) {
                        out.add(held);
                    }
                    continue;
                }
                final int at = free.indexOf(passed);
                if (at < args.size()) {
                    for (final int val : argsets.get(at)) {
                        out.add(val);
                    }
                    if (this.gapped(raw, free.size()) || this.lacking(argsets, free.size())) {
                        continue;
                    }
                    narrowed = true;
                }
            }
            if (kind == 'O' && this.state.rules.taken(new Wish('b', place, ctx, head))) {
                out.add(this.state.reduction.await(head));
            }
            if (kind == 'O' && this.gapped(raw, free.size())) {
                if (!this.state.rules.taken(new Wish('b', place, ctx, head))) {
                    this.state.rules.wish(new Wish('b', place, ctx, head));
                }
                continue;
            }
            if (kind == 'O' && this.lacking(argsets, free.size())) {
                continue;
            }
            if (kind == 'O') {
                this.bind(value, free, argsets, raw, place, tag);
                final int copy = this.state.copies.copy(
                    value.form(), place,
                    value.bound() | this.state.copies.bits(
                        value.form(), free.subList(0, Math.min(args.size(), free.size()))
                    ),
                    this.rows(head, value, free, argsets),
                    this.state.tags.tagged(tag, value.form(), place)
                );
                this.state.passes.remember(copy);
                if (!narrowed) {
                    out.add(copy);
                }
            } else if (kind == 'D') {
                out.add(this.state.values.conflict("data()"));
            } else if (kind != 'X') {
                out.add(head);
            }
        }
        this.landing(place, ctx, landed);
        return out;
    }

    /**
     * Where the arguments of each call land, over the copies around that
     * count: the one void each lands in, for those that land in one.
     *
     * @return The voids, by the place of the argument
     */
    Map<String, String> lands() {
        final Map<String, Set<String>> voids = new LinkedHashMap<>(0);
        for (final Map.Entry<Slot, Set<Landing>> entry : this.lands.entrySet()) {
            if (this.state.counting.counts(entry.getKey().copy())) {
                final List<String> args = this.state.program.args(entry.getKey().name());
                for (final Landing landing : entry.getValue()) {
                    voids.computeIfAbsent(args.get(landing.arg()), key -> new HashSet<>(1))
                        .add(landing.name());
                }
            }
        }
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Set<String>> entry : voids.entrySet()) {
            if (entry.getValue().size() == 1 && !entry.getValue().contains("")) {
                found.put(entry.getKey(), entry.getValue().iterator().next());
            }
        }
        return found;
    }

    /**
     * Put the arguments of a call through to what they are handed to: the
     * copies an atom calls them back with, Java's dataizing, the pool.
     *
     * @param value The head, a copy
     * @param free Its empty voids
     * @param argsets The arguments, reduced
     * @param raw The arguments, as worked out
     * @param place The place of the call
     * @param tag The tag of the caller
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private void bind(
        final Value value, final List<String> free, final List<int[]> argsets,
        final List<int[]> raw, final String place, final String tag
    ) {
        final String form = value.form();
        final int count = Math.min(free.size(), argsets.size());
        if (this.state.roles.returned(form)) {
            for (int arg = 0; arg < count; ++arg) {
                this.state.pool.fill(form, free.get(arg), argsets.get(arg));
            }
        }
        final Formation shape = this.state.program.form(form);
        for (int arg = 0; arg < count; ++arg) {
            if (shape.back(free.get(arg))) {
                for (final int val : raw.get(arg)) {
                    this.state.callbacks.handed(val);
                    this.state.callbacks.called(val, shape.args(free.get(arg)), place, tag);
                }
            }
        }
        if (this.state.roles.atom(form)) {
            for (int arg = 0; arg < count; ++arg) {
                if (!shape.back(free.get(arg))) {
                    for (final int val : raw.get(arg)) {
                        this.state.callbacks.handed(val);
                    }
                    for (final int val : raw.get(arg)) {
                        this.state.counting.use(
                            val, free.get(arg), place, tag, Counting.DATAIZED, new Ints()
                        );
                    }
                    for (final int val : argsets.get(arg)) {
                        this.state.counting.use(
                            val, free.get(arg), place, tag, Counting.DATAIZED, new Ints()
                        );
                    }
                }
            }
        }
    }

    /**
     * The rows of the copy a call makes: those of its head, and the
     * arguments in its empty voids.
     *
     * @param head The head
     * @param value Its value
     * @param free Its empty voids
     * @param argsets The arguments, reduced
     * @return The rows, by void
     */
    private Map<String, int[]> rows(
        final int head, final Value value, final List<String> free, final List<int[]> argsets
    ) {
        final Map<String, int[]> rows = new LinkedHashMap<>(0);
        for (final String name : this.state.values.bound(value)) {
            rows.put(name, this.state.rows.cell(head, name));
        }
        final int count = Math.min(free.size(), argsets.size());
        for (int arg = 0; arg < count; ++arg) {
            rows.put(free.get(arg), argsets.get(arg));
        }
        return rows;
    }

    /**
     * Whether an argument for one of the empty voids holds only open voids.
     *
     * @param raw The arguments, as worked out
     * @param free How many empty voids the head has
     * @return TRUE if one does
     */
    private boolean gapped(final List<int[]> raw, final int free) {
        boolean found = false;
        for (int arg = 0; arg < Math.min(free, raw.size()); ++arg) {
            final int[] set = raw.get(arg);
            if (set.length > 0) {
                boolean all = true;
                for (final int val : set) {
                    if (!this.state.values.gap(val)) {
                        all = false;
                        break;
                    }
                }
                if (all) {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    /**
     * Whether an argument for one of the empty voids has nothing yet.
     *
     * @param argsets The arguments, reduced
     * @param free How many empty voids the head has
     * @return TRUE if one has
     */
    private boolean lacking(final List<int[]> argsets, final int free) {
        boolean found = false;
        for (int arg = 0; arg < Math.min(free, argsets.size()); ++arg) {
            if (argsets.get(arg).length == 0) {
                found = true;
                break;
            }
        }
        return found;
    }

    /**
     * Note where the arguments of a call in a copy land, next to what was
     * noted before.
     *
     * @param place The place of the call
     * @param ctx The copy around
     * @param landed The landings
     */
    private void landing(final String place, final int ctx, final Set<Landing> landed) {
        final Slot key = new Slot(ctx, place);
        final Set<Landing> old = this.lands.getOrDefault(key, Collections.emptySet());
        if (!old.containsAll(landed)) {
            final Set<Landing> got = new HashSet<>(old);
            got.addAll(landed);
            this.lands.put(key, this.landings.computeIfAbsent(got, set -> set));
        }
    }
}
