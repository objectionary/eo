/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.BitSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Which copies count (design doc 3.3): a copy that counts is walked, its
 * places are answered and reduced.
 *
 * <ul>
 * <li>A copy with no empty void of its own counts.</li>
 * <li>A copy with an empty void of its own counts only if code reads from
 * it as it is, an atom without {@code args} takes it, it is dataized
 * through a test, or a stop keeps it. Each of these is a <b>use</b>,
 * found by {@code use()}, which follows the chain of {@code @} down.</li>
 * <li>A test counts as dataized.</li>
 * <li>A copy whose {@code ^} does not count does not count either: its
 * {@code ρ} row has values and every one is a copy that does not count,
 * followed up the chain, where a copy met again counts.</li>
 * </ul>
 *
 * <p>What it worked out is kept until the count of changes, the tally,
 * moves: a new value in a {@code ρ} row, a copy that began to count, a
 * study copy.</p>
 *
 * @since 0.76.0
 */
final class Counting {

    /**
     * A walk of {@code use()} that reads.
     */
    static final int READ = 0;

    /**
     * A walk of {@code use()} that dataizes.
     */
    static final int DATAIZED = 1;

    /**
     * A walk of {@code use()} that a test dataizes.
     */
    static final int TESTED = 2;

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The copies code uses as they are.
     */
    private final BitSet used;

    /**
     * Whether each value has an empty void of its own: zero for not worked
     * out, one for no, two for yes.
     */
    private final Marks bares;

    /**
     * Whether each value counts, with the tally it was worked out at: zero
     * for not worked out, else the tally twice, plus one if it counts, plus
     * one.
     */
    private final Marks counted;

    /**
     * The tally.
     */
    private final AtomicLong tally;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Counting(final State walk) {
        this.state = walk;
        this.used = new BitSet();
        this.bares = new Marks();
        this.counted = new Marks();
        this.tally = new AtomicLong();
    }

    /**
     * The tally of changes that may change which copies count.
     *
     * @return The tally
     */
    long tally() {
        return this.tally.get();
    }

    /**
     * Move the tally.
     */
    void grow() {
        this.tally.incrementAndGet();
    }

    /**
     * Whether a value is a copy made in EO code with an empty void of its
     * own.
     *
     * @param num The value
     * @return TRUE if it is
     */
    boolean bare(final int num) {
        int got = this.bares.get(num);
        if (got == 0) {
            final Value value = this.state.values().get(num);
            if (value.kind() == 'O' && value.placed()
                && !this.state.values().opened(value).isEmpty()) {
                got = 2;
            } else {
                got = 1;
            }
            this.bares.put(num, got);
        }
        return got == 2;
    }

    /**
     * Whether a value counts by itself: it has no empty void of its own, or
     * code uses it, or a stop keeps it.
     *
     * @param num The value
     * @return TRUE if it does
     */
    boolean alone(final int num) {
        return !this.bare(num) || this.used.get(num) || this.state.study().kept(num);
    }

    /**
     * Whether a value counts.
     *
     * @param num The value
     * @return TRUE if it does
     */
    boolean counts(final int num) {
        int got = this.counted.get(num);
        final long now = this.tally.get();
        if (got == 0 || (got - 1) / 2 != now) {
            got = (int) (now * 2) + 1;
            if (this.fresh(num)) {
                got += 1;
            }
            this.counted.put(num, got);
        }
        final int result = (got - 1) % 2;
        if (this.state.memo().busy()) {
            this.state.memo().watch(new Key('c', num, "", "", "", result));
        }
        return result == 1;
    }

    /**
     * Whether a count told before has turned since: the value counts now
     * and did not then, or the other way round.
     *
     * @param told The count, with what it was
     * @return TRUE if it turned
     */
    boolean turned(final Key told) {
        return this.counts(told.value()) != (told.how() == 1);
    }

    /**
     * Use a value as it is: read off, dataized by an atom, or dataized by
     * a test, down the chain of {@code @}.
     *
     * @param num The value
     * @param usage How it is used, and where
     * @param met The copies met on this chain of {@code @} so far
     */
    void use(final int num, final Usage usage, final Ints met) {
        if (!this.state.passes().frozen()) {
            final Key walk = new Key(
                'u', num, usage.name(), usage.site().place(), usage.site().tag(), usage.how()
            );
            this.state.memo().depend(walk);
            if (!this.state.memo().done(walk)) {
                this.walk(walk, usage, met);
            }
        }
    }

    /**
     * Whether a copy's {@code ^} counts, up the chain.
     *
     * @param num The copy
     * @param chain The copies met on the chain so far
     * @return TRUE if it does
     */
    boolean upheld(final int num, final Ints chain) {
        final Ints row = this.state.rows().row(num, "ρ");
        boolean result = row.isEmpty();
        for (int idx = 0; idx < row.size() && !result; ++idx) {
            result = this.state.values().get(row.get(idx)).kind() != 'O';
        }
        for (int idx = 0; idx < row.size() && !result; ++idx) {
            result = this.holds(row.get(idx), chain);
        }
        return result;
    }

    private void walk(final Key walk, final Usage usage, final Ints met) {
        final long hits = this.state.memo().hits();
        this.state.memo().enter(walk);
        try {
            this.now(walk.value(), usage, met);
        } finally {
            this.state.memo().leave();
        }
        if (this.state.memo().hits() == hits) {
            this.state.memo().finish(walk);
        }
    }

    private boolean fresh(final int num) {
        return this.state.values().get(num).kind() != 'O'
            || this.alone(num) && (this.kept(num) || this.upheld(num, new Ints(num)));
    }

    private boolean kept(final int num) {
        return this.state.roles().test(this.state.values().get(num).form())
            || this.state.study().kept(num);
    }

    private boolean holds(final int outer, final Ints chain) {
        return chain.contains(outer) || this.alone(outer)
            && (this.kept(outer) || this.upheld(outer, Counting.longer(chain, outer)));
    }

    private void now(final int num, final Usage usage, final Ints met) {
        final int inside = this.state.copies().joined(this.state.values().bare(num));
        final Value value = this.state.values().get(inside);
        if (value.kind() == 'O') {
            if (this.bare(inside)) {
                this.touch(inside);
            }
            if (usage.how() == Counting.READ) {
                this.read(inside, usage, met);
            } else {
                this.dataized(inside, usage, met);
            }
        }
    }

    private void touch(final int inside) {
        if (!this.alone(inside)) {
            this.grow();
            this.state.passes().change();
            this.state.passes().requeue(inside);
        }
        this.used.set(inside);
    }

    private void dataized(final int inside, final Usage usage, final Ints met) {
        final Value value = this.state.values().get(inside);
        if (value.placed() && this.state.program().form(value.form()).has("φ")
            && met.add(inside)) {
            this.further(
                this.state.reads().attr(
                    inside, "φ", new Site(String.join(".", value.form(), "φ"), value.tag())
                ),
                usage, met
            );
        }
    }

    private void read(final int inside, final Usage usage, final Ints met) {
        final Value value = this.state.values().get(inside);
        final Formation shape = this.state.program().form(value.form());
        if (!"ρ".equals(usage.name()) && !Values.JBORN.equals(value.birth())
            && !shape.has(usage.name()) && met.add(inside)) {
            final List<String> behind;
            if (shape.has("φ")) {
                behind = List.of("φ");
            } else {
                behind = this.state.roles().own(value.form());
            }
            for (final String own : behind) {
                this.further(this.state.reads().attr(inside, own, usage.site()), usage, met);
            }
        }
    }

    private void further(final Ids got, final Usage usage, final Ints met) {
        for (int idx = 0; idx < got.size(); ++idx) {
            this.use(got.get(idx), usage, met);
        }
    }

    private static Ints longer(final Ints chain, final int num) {
        final Ints more = new Ints(chain.array());
        more.add(num);
        return more;
    }
}
