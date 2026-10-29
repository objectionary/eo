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
    private final BitSet study;

    /**
     * The study copies of each formation.
     */
    private final Map<String, List<Integer>> of;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Study(final State walk) {
        this.state = walk;
        this.written = new LinkedHashMap<>(0);
        this.studied = new LinkedHashMap<>(0);
        this.stays = new BitSet();
        this.study = new BitSet();
        this.of = new HashMap<>(0);
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
     * Stop w: the formations no copy calls with a void no copy fills, each
     * with those voids.
     *
     * @return The formations, in the order of the facts
     */
    Map<String, List<String>> stays() {
        final Map<String, Long> filled = new HashMap<>(0);
        final Pile all = this.state.passes.contexts();
        for (int idx = 0; idx < all.size(); ++idx) {
            final int ctx = all.get(idx);
            final Value value = this.state.values.get(ctx);
            if (value.kind() == 'O' && this.state.copies.placed(value)) {
                filled.merge(value.form(), value.bound(), (left, right) -> left | right);
            } else if (value.kind() == 'O' && Values.JBORN.equals(value.birth())
                && this.state.roles.returned(value.form())) {
                filled.merge(
                    value.form(), this.state.copies.mask(value.form()),
                    (left, right) -> left | right
                );
            }
        }
        final Map<String, Ints> real = this.reals();
        final Map<String, List<String>> out = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Integer> entry : this.written.entrySet()) {
            final String form = entry.getKey();
            final int copy = entry.getValue();
            if (this.studied.containsKey(form) || real.containsKey(form)
                || !this.state.counting.bare(copy) || this.state.counting.alone(copy)) {
                continue;
            }
            final List<String> never = new ArrayList<>(0);
            final Formation shape = this.state.program.form(form);
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
        return out;
    }

    /**
     * Note the formations stop w picked.
     *
     * @param picked The formations, each with the voids no copy fills
     */
    void pick(final Map<String, List<String>> picked) {
        this.studied.putAll(picked);
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
            final String up = this.state.program.nearest(form);
            final List<Integer> hosts = new ArrayList<>(0);
            if (!up.isEmpty()) {
                final Ints got = real.getOrDefault(up, new Ints());
                if (!got.isEmpty()) {
                    for (int idx = 0; idx < got.size(); ++idx) {
                        hosts.add(got.get(idx));
                    }
                } else {
                    hosts.addAll(this.of.getOrDefault(up, new ArrayList<>(0)));
                }
            }
            hosts.sort(Comparator.comparing(this.state.values::full, order));
            if (hosts.isEmpty()) {
                final int copy = this.written.get(form);
                if (!this.stays.get(copy)) {
                    this.stays.set(copy);
                    out.push(copy);
                }
                continue;
            }
            for (final int host : hosts) {
                final int copy = this.state.copies.copy(
                    form, String.format("in:%s", this.state.values.full(host)),
                    Formation.RHO, Map.of("ρ", new int[] {host}),
                    this.state.values.get(host).tag()
                );
                if (this.state.values.get(copy).kind() == 'O' && !this.stays.get(copy)) {
                    this.stays.set(copy);
                    this.study.set(copy);
                    this.of.computeIfAbsent(form, key -> new ArrayList<>(1)).add(copy);
                    out.push(copy);
                }
            }
        }
        for (int idx = 0; idx < out.size(); ++idx) {
            this.state.counting.grow();
            this.state.passes.requeue(out.get(idx));
        }
        return out.size();
    }

    /**
     * The real copies of each formation: those that count and are neither
     * as written nor study copies.
     *
     * @return The copies, by formation
     */
    private Map<String, Ints> reals() {
        final Map<String, Ints> found = new HashMap<>(0);
        final Pile all = this.state.passes.contexts();
        for (int idx = 0; idx < all.size(); ++idx) {
            final int ctx = all.get(idx);
            final Value value = this.state.values.get(ctx);
            if (value.kind() == 'O'
                && (!value.birth().equals(value.form()) || !value.tag().isEmpty())
                && !this.study.get(ctx) && this.state.counting.counts(ctx)) {
                found.computeIfAbsent(value.form(), key -> new Ints())
                    .add(this.state.copies.joined(ctx));
            }
        }
        return found;
    }
}
