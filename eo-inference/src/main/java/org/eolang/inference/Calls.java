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
     * The world the walk goes over.
     */
    private final World world;

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
     * @param world The world the walk goes over
     */
    Calls(final State walk, final World world) {
        this.state = walk;
        this.world = world;
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
        final List<String> args = this.world.args(place);
        final List<int[]> raw = new ArrayList<>(args.size());
        for (int arg = 0; arg < args.size(); ++arg) {
            raw.add(this.argument(args.get(arg), arg, ctx, place));
        }
        final List<int[]> argsets = new ArrayList<>(args.size());
        for (final int[] set : raw) {
            argsets.add(this.state.reduced(set));
        }
        for (final int[] set : argsets) {
            for (final int val : set) {
                this.state.remember(val);
            }
        }
        final Call call = new Call(
            new Site(place, this.world.carried(ctx)), ctx, raw, argsets
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
            if (this.state.counts(entry.getKey().copy())) {
                final List<String> args = this.world.args(entry.getKey().name());
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
        final Ids got = this.state.evaluate(arg, ctx);
        final Ints kept = new Ints();
        for (int idx = 0; idx < got.size(); ++idx) {
            if (this.world.value(got.get(idx)).kind() != 'X') {
                kept.add(got.get(idx));
            }
        }
        final Wish wish = new Wish('3', place, ctx, pos);
        if (!got.isEmpty() && this.state.taken(wish)) {
            kept.add(this.world.unknown("can't tell"));
        } else if (!got.isEmpty() && kept.isEmpty()) {
            this.state.wish(wish);
        }
        return kept.array();
    }

    private Ints head(final int num, final Call call, final Set<Landing> landed) {
        int head = num;
        Value value = this.world.value(head);
        if (value.kind() == 'B') {
            head = value.base();
            value = this.world.value(head);
        }
        final List<String> voids = this.voids(value, call);
        this.land(head, value, voids, call.size(), landed);
        final char kind = value.kind();
        final Ints out = new Ints();
        if (kind == 'A') {
            out.add(this.marked(value, call.size()));
        } else if (kind == 'O') {
            out.addAll(this.object(head, value, call, voids));
        } else if (kind == 'D') {
            out.add(this.world.conflict("data()"));
        } else if (kind != 'X') {
            out.add(head);
        }
        return out;
    }

    private List<String> voids(final Value value, final Call call) {
        final List<String> voids;
        if (value.kind() == 'O') {
            voids = this.world.voids(value, call.site().place());
        } else {
            voids = Collections.emptyList();
        }
        return voids;
    }

    private void land(
        final int head, final Value value, final List<String> voids, final int args,
        final Set<Landing> landed
    ) {
        final char kind = value.kind();
        if (kind == 'O' && this.fits(head, value, voids, args)) {
            for (int arg = 0; arg < args; ++arg) {
                landed.add(new Landing(arg, voids.get(arg)));
            }
        } else if (kind != 'O' && kind != 'T' && kind != 'D' && kind != 'X') {
            for (int arg = 0; arg < args; ++arg) {
                landed.add(new Landing(arg, ""));
            }
        }
    }

    private boolean fits(
        final int head, final Value value, final List<String> voids, final int args
    ) {
        return !Values.JBORN.equals(value.birth()) && !this.state.shared(head)
            && voids.size() == args;
    }

    private int marked(final Value value, final int args) {
        final int result;
        if (args < value.count()) {
            result = this.state.mark(value.base(), value.count() - args);
        } else if (args == value.count()) {
            final Value base = this.world.value(value.base());
            if (base.kind() == 'O' && !this.world.opened(base).isEmpty()) {
                result = this.state.mark(value.base(), 0);
            } else {
                result = value.base();
            }
        } else {
            result = Values.TERM;
        }
        return result;
    }

    private Ints object(
        final int head, final Value value, final Call call, final List<String> voids
    ) {
        final String passed = this.world.passes(value.form());
        final Ints out;
        if ((Values.JBORN.equals(value.birth()) || this.state.shared(head))
            && call.size() > 0 || voids.size() < call.size()) {
            out = new Ints(Values.TERM);
        } else if (passed.isEmpty()) {
            out = this.made(head, value, call, voids, false);
        } else if ((value.bound() & this.world.form(value.form()).bit(passed)) == 0) {
            out = this.narrowed(head, value, call, voids, voids.indexOf(passed));
        } else {
            out = new Ints(this.state.cell(head, passed));
        }
        return out;
    }

    private Ints narrowed(
        final int head, final Value value, final Call call, final List<String> voids,
        final int pos
    ) {
        final int free = this.world.opened(value).size();
        final Ints out = new Ints();
        if (pos >= 0) {
            for (final int val : call.sets().get(pos)) {
                out.add(val);
            }
            if (!this.gapped(call.raw(), free) && !call.lacking(free)) {
                out.addAll(this.made(head, value, call, voids, true));
            }
        } else {
            out.addAll(this.made(head, value, call, voids, false));
        }
        return out;
    }

    private Ints made(
        final int head, final Value value, final Call call, final List<String> voids,
        final boolean narrowed
    ) {
        final List<String> free = this.world.opened(value);
        final Wish wish = new Wish('b', call.site().place(), call.ctx(), head);
        final Ints out = new Ints();
        if (this.state.taken(wish)) {
            out.add(this.state.await(head));
        }
        if (this.gapped(call.raw(), free.size())) {
            if (!this.state.taken(wish)) {
                this.state.wish(wish);
            }
        } else if (!call.lacking(free.size())) {
            this.bind(value, voids, call);
            final int copy = this.state.copy(
                new Value(
                    value.form(), this.world.born(call.site(), value.form()),
                    value.bound() | this.state.bits(value.form(), voids)
                ),
                this.rows(head, value, voids, call.sets())
            );
            this.state.remember(copy);
            if (!narrowed) {
                out.add(copy);
            }
        }
        return out;
    }

    private void bind(final Value value, final List<String> voids, final Call call) {
        final String form = value.form();
        if (this.world.returned(form)) {
            for (int arg = 0; arg < voids.size(); ++arg) {
                this.state.fill(form, voids.get(arg), call.sets().get(arg));
            }
        }
        final Formation shape = this.world.form(form);
        for (int arg = 0; arg < voids.size(); ++arg) {
            if (shape.back(voids.get(arg))) {
                this.back(voids.get(arg), arg, shape, call);
            }
        }
        if (this.world.atom(form)) {
            for (int arg = 0; arg < voids.size(); ++arg) {
                if (!shape.back(voids.get(arg))) {
                    this.dataized(voids.get(arg), arg, call);
                }
            }
        }
    }

    private void back(final String name, final int arg, final Formation shape, final Call call) {
        for (final int val : call.raw().get(arg)) {
            this.state.handed(val);
            this.state.called(val, shape.args(name), call.site());
        }
    }

    private void dataized(final String name, final int arg, final Call call) {
        final Usage usage = new Usage(name, call.site(), Counting.DATAIZED);
        for (final int val : call.raw().get(arg)) {
            this.state.handed(val);
        }
        for (final int val : call.raw().get(arg)) {
            this.state.use(val, usage, new Ints());
        }
        for (final int val : call.sets().get(arg)) {
            this.state.use(val, usage, new Ints());
        }
    }

    private Map<String, int[]> rows(
        final int head, final Value value, final List<String> voids, final List<int[]> argsets
    ) {
        final Map<String, int[]> rows = new LinkedHashMap<>(0);
        for (final String name : this.world.bound(value)) {
            rows.put(name, this.state.cell(head, name));
        }
        for (int arg = 0; arg < voids.size(); ++arg) {
            rows.put(voids.get(arg), argsets.get(arg));
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
            if (!this.world.gap(val)) {
                all = false;
                break;
            }
        }
        return all;
    }

    private void landing(final String place, final int ctx, final Set<Landing> landed) {
        final Set<Landing> old = this.lands.getOrDefault(
            new Slot(ctx, place), Collections.emptySet()
        );
        if (!old.containsAll(landed)) {
            final Set<Landing> got = new HashSet<>(old);
            got.addAll(landed);
            this.lands.put(new Slot(ctx, place), this.landings.computeIfAbsent(got, set -> set));
        }
    }
}
