/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Study copies (design doc 3.4): the copies stop w gives the formations
 * nobody calls.
 *
 * <p>Stop w picks the formations that no copy calls and whose copy as
 * written does not count, for one of two reasons: a void of its own that no
 * copy fills, or a {@code ^} that does not count. It picks them in rounds.
 * One of them waits while the formation right around it is one of them too,
 * or while a place in another one of them reads it as an attribute: the
 * others are picked and walked first, and the next stop looks again, where
 * one that has got a real copy since is no longer picked. If every one of
 * them waits, all are picked at once.</p>
 *
 * <p>A formation stop w picks gets one study copy inside every real
 * copy of the formation right around it: one that counts and is neither
 * the copy as written nor a study copy. The study copy is born
 * {@code in:H}, with {@code H} the real copy, which is its {@code ^} and
 * only one when it declares {@code ^}; one that declares none has no
 * {@code ^} at all (#9785). A formation with no name of its own takes as
 * its {@code ^} only the object it goes into, which a study copy stands
 * for but does not have, so its {@code ^} stays empty (#9655). Its own
 * voids stay empty. If the formation around has no real copy, the study
 * copies go inside its study copies; if
 * it has neither, or nothing is around, the copy as written is kept. Every
 * later stop gives a study copy to each real copy that has appeared since,
 * and a kept copy stays kept, and counts.</p>
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
     * Stop w: pick, for this round, the formations no copy calls whose copy
     * as written does not count, each with the voids no copy fills.
     *
     * @return TRUE if it picked any
     */
    boolean pick() {
        final Map<String, List<String>> found = this.stays();
        final Map<String, List<String>> picked = new LinkedHashMap<>(found);
        picked.keySet().removeAll(this.waiting(found.keySet()));
        if (picked.isEmpty()) {
            picked.putAll(found);
        }
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
                if (!never.isEmpty() || this.state.alone(entry.getValue())) {
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
            && !this.state.counts(copy);
    }

    private Set<String> waiting(final Set<String> forms) {
        final Set<String> found = new HashSet<>(0);
        for (final String form : forms) {
            if (forms.contains(this.world.nearest(form))) {
                found.add(form);
            }
            for (final String place : this.world.body(form)) {
                final String read = this.copied(place);
                if (!read.equals(form) && forms.contains(read)) {
                    found.add(read);
                }
            }
        }
        return found;
    }

    private String copied(final String place) {
        String found = "";
        if (!this.world.starts(place) && this.world.reads(place)
            && !"ρ".equals(this.world.read(place).name())) {
            found = this.held(place);
        }
        return found;
    }

    private String held(final String place) {
        final List<Head> heads = this.world.heads(place);
        String found = "";
        if (heads.size() == 1 && "ref".equals(heads.get(0).tag())) {
            found = heads.get(0).loc();
        } else if (!this.world.starts(place) && this.world.reads(place)) {
            final Read read = this.world.read(place);
            final String outer = this.held(read.of());
            final String inner = String.join(".", outer, read.name());
            if ("ρ".equals(read.name()) && this.world.formation(outer)) {
                found = this.world.form(outer).parent();
            } else if (!outer.isEmpty() && this.world.formation(inner)) {
                found = inner;
            }
        }
        return found;
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
        final Map<String, int[]> rows;
        if (this.world.form(form).applied()) {
            rows = Map.of();
        } else {
            rows = Map.of("ρ", new int[] {host});
        }
        final int copy = this.state.copy(
            new Value(
                form,
                new Site(
                    String.format("in:%s", this.world.full(host)),
                    this.world.value(host).tag()
                ),
                Formation.RHO
            ),
            rows
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
