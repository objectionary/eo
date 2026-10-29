/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Study copies (design doc 3.4): the copies stop w gives the formations
 * nobody calls.
 *
 * <p>Stop w picks the formations that no copy calls and that have a void
 * no copy fills. Such a formation gets one study copy inside every real
 * copy of the formation right around it: one that counts and is neither
 * the copy as written nor a study copy. The study copy is born
 * {@code in:H}, with {@code H} the real copy, which is its {@code ^} and
 * only one; its own voids stay empty. If the formation around has no real
 * copy, the study copies go inside its study copies; if it has neither,
 * or nothing is around, the copy as written is kept. Every later stop
 * gives a study copy to each real copy that has appeared since, and a kept
 * copy stays kept, and counts.</p>
 *
 * @since 0.76.0
 */
final class Study {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The world the walk goes over.
     */
    private final World world;

    /**
     * The copy as written of each formation, in the order of the facts.
     */
    private final Map<String, Integer> written;

    /**
     * The formations stop w picked, with the voids no copy fills.
     */
    private final Map<String, List<String>> studied;

    /**
     * The copies a stop keeps: study copies and copies as written.
     */
    private final BitSet stays;

    /**
     * The study copies.
     */
    private final BitSet samples;

    /**
     * The study copies of each formation.
     */
    private final Map<String, List<Integer>> grouped;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Study(final State walk, final World world) {
        this.state = walk;
        this.world = world;
        this.written = new LinkedHashMap<>(0);
        this.studied = new LinkedHashMap<>(0);
        this.stays = new BitSet();
        this.samples = new BitSet();
        this.grouped = new HashMap<>(0);
    }

    /**
     * Note the copy as written of a formation.
     *
     * @param form The formation
     * @param copy Its copy as written
     */
    void write(final String form, final int copy) {
        this.written.put(form, copy);
    }

    /**
     * Whether a stop keeps a copy.
     *
     * @param num The copy
     * @return TRUE if it does
     */
    boolean kept(final int num) {
        return this.stays.get(num);
    }

    /**
     * Stop w: pick the formations no copy calls with a void no copy fills,
     * each with those voids.
     *
     * @return TRUE if it picked any
     */
    boolean pick() {
        final Map<String, List<String>> picked = this.stays();
        this.studied.putAll(picked);
        return !picked.isEmpty();
    }

    /**
     * Give each formation stop w picked its study copies inside every real
     * copy around it that has none yet, or keep its copy as written.
     *
     * @return How many copies were kept
     */
    int studied() {
        final Map<String, Ints> real = this.reals();
        final List<String> forms = new ArrayList<>(this.studied.keySet());
        final CodeOrder order = new CodeOrder();
        forms.sort(
            Comparator.<String>comparingInt(form -> form.length() - form.replace(".", "").length())
                .thenComparing(order)
        );
        final Pile out = new Pile();
        for (final String form : forms) {
            final List<Integer> hosts = this.hosts(form, real);
            hosts.sort(Comparator.comparing(this.world::full, order));
            if (hosts.isEmpty()) {
                this.keep(this.written.get(form), out);
            }
            for (final int host : hosts) {
                this.sample(form, host, out);
            }
        }
        for (int idx = 0; idx < out.size(); ++idx) {
            this.state.grow();
            this.state.requeue(out.get(idx));
        }
        return out.size();
    }

    private Map<String, List<String>> stays() {
        final Map<String, Long> filled = this.filled();
        final Map<String, Ints> real = this.reals();
        final Map<String, List<String>> out = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Integer> entry : this.written.entrySet()) {
            final String form = entry.getKey();
            if (this.lonely(form, entry.getValue(), real)) {
                final List<String> never = new ArrayList<>(0);
                final Formation shape = this.world.form(form);
                final long mask = filled.getOrDefault(form, 0L);
                for (final String name : shape.order()) {
                    if ((mask & shape.bit(name)) == 0) {
                        never.add(name);
                    }
                }
                if (!never.isEmpty()) {
                    out.put(form, never);
                }
            }
        }
        return out;
    }

    private Map<String, Long> filled() {
        final Map<String, Long> filled = new HashMap<>(0);
        final Pile all = this.state.contexts();
        for (int idx = 0; idx < all.size(); ++idx) {
            final Value value = this.world.value(all.get(idx));
            if (value.kind() == 'O' && value.placed()) {
                filled.merge(value.form(), value.bound(), (left, right) -> left | right);
            } else if (value.kind() == 'O' && Values.JBORN.equals(value.birth())
                && this.world.returned(value.form())) {
                filled.merge(
                    value.form(), this.state.mask(value.form()),
                    (left, right) -> left | right
                );
            }
        }
        return filled;
    }

    private boolean lonely(final String form, final int copy, final Map<String, Ints> real) {
        return !this.studied.containsKey(form) && !real.containsKey(form)
            && this.state.bare(copy) && !this.state.alone(copy);
    }

    private List<Integer> hosts(final String form, final Map<String, Ints> real) {
        final String outer = this.world.nearest(form);
        final List<Integer> hosts = new ArrayList<>(0);
        final Ints got = real.getOrDefault(outer, new Ints());
        if (!outer.isEmpty() && got.isEmpty()) {
            hosts.addAll(this.grouped.getOrDefault(outer, new ArrayList<>(0)));
        } else if (!outer.isEmpty()) {
            for (int idx = 0; idx < got.size(); ++idx) {
                hosts.add(got.get(idx));
            }
        }
        return hosts;
    }

    private void keep(final int copy, final Pile out) {
        if (!this.stays.get(copy)) {
            this.stays.set(copy);
            out.push(copy);
        }
    }

    private void sample(final String form, final int host, final Pile out) {
        final int copy = this.state.copy(
            new Value(
                form,
                new Site(
                    String.format("in:%s", this.world.full(host)),
                    this.world.value(host).tag()
                ),
                Formation.RHO
            ),
            Map.of("ρ", new int[] {host})
        );
        if (this.world.value(copy).kind() == 'O' && !this.stays.get(copy)) {
            this.stays.set(copy);
            this.samples.set(copy);
            this.grouped.computeIfAbsent(form, key -> new ArrayList<>(1)).add(copy);
            out.push(copy);
        }
    }

    private Map<String, Ints> reals() {
        final Map<String, Ints> found = new HashMap<>(0);
        final Pile all = this.state.contexts();
        for (int idx = 0; idx < all.size(); ++idx) {
            final int ctx = all.get(idx);
            final Value value = this.world.value(ctx);
            if (value.kind() == 'O' && Study.real(value)
                && !this.samples.get(ctx) && this.state.counts(ctx)) {
                found.computeIfAbsent(value.form(), key -> new Ints())
                    .add(this.state.joined(ctx));
            }
        }
        return found;
    }

    private static boolean real(final Value value) {
        return !value.birth().equals(value.form()) || !value.tag().isEmpty();
    }
}
