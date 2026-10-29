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
     * Apply heads to the arguments of the call at a place.
     *
     * @param heads What the place gives before the call
     * @param ctx The copy around
     * @param place The place of the call
     * @return What the call gives
     */
    Ints apply(final Ints heads, final int ctx, final String place) {
        final List<String> args = this.state.program().args(place);
        final List<int[]> raw = new ArrayList<>(args.size());
        for (int arg = 0; arg < args.size(); ++arg) {
            raw.add(this.argument(args.get(arg), arg, ctx, place));
        }
        final List<int[]> argsets = new ArrayList<>(args.size());
        for (final int[] set : raw) {
            argsets.add(this.state.reduction().reduced(set));
        }
        for (final int[] set : argsets) {
            for (final int val : set) {
                this.state.passes().remember(val);
            }
        }
        final Call call = new Call(
            new Site(place, this.state.tags().carried(ctx)), ctx, raw, argsets
        );
        final Ints out = new Ints();
        final Set<Landing> landed = new HashSet<>(0);
        for (int pos = 0; pos < heads.size(); ++pos) {
            out.addAll(this.head(heads.get(pos), call, landed));
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
            if (this.state.counting().counts(entry.getKey().copy())) {
                final List<String> args = this.state.program().args(entry.getKey().name());
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

    private int[] argument(final String arg, final int pos, final int ctx, final String place) {
        final Ids got = this.state.places().evaluate(arg, ctx);
        final Ints kept = new Ints();
        for (int idx = 0; idx < got.size(); ++idx) {
            if (this.state.values().get(got.get(idx)).kind() != 'X') {
                kept.add(got.get(idx));
            }
        }
        final Wish wish = new Wish('3', place, ctx, pos);
        if (!got.isEmpty() && this.state.rules().taken(wish)) {
            kept.add(this.state.values().unknown("can't tell"));
        } else if (!got.isEmpty() && kept.isEmpty()) {
            this.state.rules().wish(wish);
        }
        return kept.array();
    }

    private Ints head(final int num, final Call call, final Set<Landing> landed) {
        int head = num;
        Value value = this.state.values().get(head);
        if (value.kind() == 'B') {
            head = value.base();
            value = this.state.values().get(head);
        }
        this.land(head, value, call.size(), landed);
        final char kind = value.kind();
        final Ints out = new Ints();
        if (kind == 'A') {
            out.add(this.marked(value, call.size()));
        } else if (kind == 'O') {
            out.addAll(this.object(head, value, call));
        } else if (kind == 'D') {
            out.add(this.state.values().conflict("data()"));
        } else if (kind != 'X') {
            out.add(head);
        }
        return out;
    }

    private void land(
        final int head, final Value value, final int args, final Set<Landing> landed
    ) {
        final char kind = value.kind();
        if (kind == 'O' && this.fits(head, value, args)) {
            final List<String> free = this.state.values().opened(value);
            for (int arg = 0; arg < args; ++arg) {
                landed.add(new Landing(arg, free.get(arg)));
            }
        } else if (kind != 'O' && kind != 'T' && kind != 'D' && kind != 'X') {
            for (int arg = 0; arg < args; ++arg) {
                landed.add(new Landing(arg, ""));
            }
        }
    }

    private boolean fits(final int head, final Value value, final int args) {
        return !Values.JBORN.equals(value.birth()) && !this.state.copies().shared(head)
            && args <= this.state.values().opened(value).size();
    }

    private int marked(final Value value, final int args) {
        final int result;
        if (args < value.count()) {
            result = this.state.reduction().mark(value.base(), value.count() - args);
        } else if (args == value.count()) {
            final Value base = this.state.values().get(value.base());
            if (base.kind() == 'O' && !this.state.values().opened(base).isEmpty()) {
                result = this.state.reduction().mark(value.base(), 0);
            } else {
                result = value.base();
            }
        } else {
            result = Values.TERM;
        }
        return result;
    }

    private Ints object(final int head, final Value value, final Call call) {
        final List<String> free = this.state.values().opened(value);
        final String passed = this.state.roles().passes(value.form());
        final Ints out;
        if ((Values.JBORN.equals(value.birth()) || this.state.copies().shared(head))
            && call.size() > 0 || call.size() > free.size()) {
            out = new Ints(Values.TERM);
        } else if (passed.isEmpty()) {
            out = this.made(head, value, call, false);
        } else if ((value.bound() & this.state.program().form(value.form()).bit(passed)) == 0) {
            out = this.narrowed(head, value, call, free.indexOf(passed));
        } else {
            out = new Ints(this.state.rows().cell(head, passed));
        }
        return out;
    }

    private Ints narrowed(final int head, final Value value, final Call call, final int pos) {
        final int free = this.state.values().opened(value).size();
        final Ints out = new Ints();
        if (pos < call.size()) {
            for (final int val : call.sets().get(pos)) {
                out.add(val);
            }
            if (!this.gapped(call.raw(), free) && !call.lacking(free)) {
                out.addAll(this.made(head, value, call, true));
            }
        } else {
            out.addAll(this.made(head, value, call, false));
        }
        return out;
    }

    private Ints made(final int head, final Value value, final Call call, final boolean narrowed) {
        final List<String> free = this.state.values().opened(value);
        final Wish wish = new Wish('b', call.site().place(), call.ctx(), head);
        final Ints out = new Ints();
        if (this.state.rules().taken(wish)) {
            out.add(this.state.reduction().await(head));
        }
        if (this.gapped(call.raw(), free.size())) {
            if (!this.state.rules().taken(wish)) {
                this.state.rules().wish(wish);
            }
        } else if (!call.lacking(free.size())) {
            this.bind(value, free, call);
            final int copy = this.state.copies().copy(
                new Value(
                    value.form(), this.state.tags().born(call.site(), value.form()),
                    value.bound() | this.state.copies().bits(
                        value.form(), free.subList(0, Math.min(call.size(), free.size()))
                    )
                ),
                this.rows(head, value, free, call.sets())
            );
            this.state.passes().remember(copy);
            if (!narrowed) {
                out.add(copy);
            }
        }
        return out;
    }

    private void bind(final Value value, final List<String> free, final Call call) {
        final String form = value.form();
        final int count = Math.min(free.size(), call.size());
        if (this.state.roles().returned(form)) {
            for (int arg = 0; arg < count; ++arg) {
                this.state.pool().fill(form, free.get(arg), call.sets().get(arg));
            }
        }
        final Formation shape = this.state.program().form(form);
        for (int arg = 0; arg < count; ++arg) {
            if (shape.back(free.get(arg))) {
                this.back(free.get(arg), arg, shape, call);
            }
        }
        if (this.state.roles().atom(form)) {
            for (int arg = 0; arg < count; ++arg) {
                if (!shape.back(free.get(arg))) {
                    this.dataized(free.get(arg), arg, call);
                }
            }
        }
    }

    private void back(final String name, final int arg, final Formation shape, final Call call) {
        for (final int val : call.raw().get(arg)) {
            this.state.callbacks().handed(val);
            this.state.callbacks().called(val, shape.args(name), call.site());
        }
    }

    private void dataized(final String name, final int arg, final Call call) {
        final Usage usage = new Usage(name, call.site(), Counting.DATAIZED);
        for (final int val : call.raw().get(arg)) {
            this.state.callbacks().handed(val);
        }
        for (final int val : call.raw().get(arg)) {
            this.state.counting().use(val, usage, new Ints());
        }
        for (final int val : call.sets().get(arg)) {
            this.state.counting().use(val, usage, new Ints());
        }
    }

    private Map<String, int[]> rows(
        final int head, final Value value, final List<String> free, final List<int[]> argsets
    ) {
        final Map<String, int[]> rows = new LinkedHashMap<>(0);
        for (final String name : this.state.values().bound(value)) {
            rows.put(name, this.state.rows().cell(head, name));
        }
        final int count = Math.min(free.size(), argsets.size());
        for (int arg = 0; arg < count; ++arg) {
            rows.put(free.get(arg), argsets.get(arg));
        }
        return rows;
    }

    private boolean gapped(final List<int[]> raw, final int free) {
        boolean found = false;
        for (int arg = 0; arg < Math.min(free, raw.size()) && !found; ++arg) {
            found = this.gaps(raw.get(arg));
        }
        return found;
    }

    private boolean gaps(final int... set) {
        boolean all = set.length > 0;
        for (final int val : set) {
            if (!this.state.values().gap(val)) {
                all = false;
                break;
            }
        }
        return all;
    }

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
