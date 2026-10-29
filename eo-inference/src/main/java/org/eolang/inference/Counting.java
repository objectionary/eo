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
 *   <li>A copy with no empty void of its own counts.</li>
 *   <li>A copy with an empty void of its own counts only if code reads from
 *   it as it is, an atom without {@code args} takes it, it is dataized
 *   through a test, or a stop keeps it. Each of these is a <b>use</b>,
 *   found by {@code use()}, which follows the chain of {@code @} down.</li>
 *   <li>A test counts as dataized.</li>
 *   <li>A copy whose {@code ^} does not count does not count either: its
 *   {@code ρ} row has values and every one is a copy that does not count,
 *   followed up the chain, where a copy met again counts.</li>
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
            final Value value = this.state.values.get(num);
            if (value.kind() == 'O' && this.state.copies.placed(value)
                && !this.state.values.opened(value).isEmpty()) {
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
        return !this.bare(num) || this.used.get(num) || this.state.study.kept(num);
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
            final Value value = this.state.values.get(num);
            final boolean yes = value.kind() != 'O' || this.alone(num)
                && (this.state.roles.test(value.form()) || this.state.study.kept(num)
                || this.upheld(num, new Ints(num)));
            got = (int) (now * 2) + (yes ? 1 : 0) + 1;
            this.counted.put(num, got);
        }
        final boolean result = (got - 1) % 2 == 1;
        if (this.state.memo.busy()) {
            this.state.memo.watch(new Key('c', num, "", "", "", result ? 1 : 0));
        }
        return result;
    }

    /**
     * Use a value as it is at a place: read off, dataized by an atom, or
     * dataized by a test, down the chain of {@code @}.
     *
     * @param num The value
     * @param name The name read, or the void it is handed to
     * @param place The place
     * @param tag The tag of the reader, empty when none
     * @param how How it is used
     * @param met The copies met on this chain of {@code @} so far
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    void use(
        final int num, final String name, final String place, final String tag,
        final int how, final Ints met
    ) {
        if (!this.state.passes.frozen()) {
            final Key walk = new Key('u', num, name, place, tag, how);
            this.state.memo.depend(walk);
            if (!this.state.memo.done(walk)) {
                final long hits = this.state.memo.hits();
                this.state.memo.enter(walk);
                try {
                    this.now(num, name, place, tag, how, met);
                } finally {
                    this.state.memo.leave();
                }
                if (this.state.memo.hits() == hits) {
                    this.state.memo.finish(walk);
                }
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
        final Ints row = this.state.rows.row(num, "ρ");
        boolean result = row.isEmpty();
        if (!result) {
            for (int idx = 0; idx < row.size(); ++idx) {
                if (this.state.values.get(row.get(idx)).kind() != 'O') {
                    result = true;
                    break;
                }
            }
        }
        if (!result) {
            for (int idx = 0; idx < row.size(); ++idx) {
                final int up = row.get(idx);
                if (chain.contains(up)) {
                    result = true;
                    break;
                }
                if (this.alone(up) && (this.state.roles.test(this.state.values.get(up).form())
                    || this.state.study.kept(up) || this.upheld(up, Counting.longer(chain, up)))) {
                    result = true;
                    break;
                }
            }
        }
        return result;
    }

    /**
     * Use a value, the walk itself.
     *
     * @param num The value
     * @param name The name read, or the void it is handed to
     * @param place The place
     * @param tag The tag of the reader, empty when none
     * @param how How it is used
     * @param met The copies met on this chain of {@code @} so far
     * @checkstyle ParameterNumberCheck (5 lines)
     * @checkstyle CyclomaticComplexityCheck (60 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    private void now(
        final int num, final String name, final String place, final String tag,
        final int how, final Ints met
    ) {
        final int inside = this.state.copies.joined(this.state.values.bare(num));
        final Value value = this.state.values.get(inside);
        if (value.kind() == 'O') {
            if (this.bare(inside)) {
                if (!this.alone(inside)) {
                    this.grow();
                    this.state.passes.change();
                    this.state.passes.requeue(inside);
                }
                this.used.set(inside);
            }
            final Formation shape = this.state.program.form(value.form());
            if (how != Counting.READ) {
                if (this.state.copies.placed(value) && shape.has("φ") && met.add(inside)) {
                    final Ids got = this.state.reads.attr(
                        inside, "φ", String.join(".", value.form(), "φ"), value.tag()
                    );
                    for (int idx = 0; idx < got.size(); ++idx) {
                        this.use(got.get(idx), name, place, tag, how, met);
                    }
                }
            } else if (!"ρ".equals(name) && !Values.JBORN.equals(value.birth())
                && !shape.has(name) && met.add(inside)) {
                final List<String> behind;
                if (shape.has("φ")) {
                    behind = List.of("φ");
                } else {
                    behind = this.state.roles.own(value.form());
                }
                for (final String own : behind) {
                    final Ids got = this.state.reads.attr(inside, own, place, tag);
                    for (int idx = 0; idx < got.size(); ++idx) {
                        this.use(got.get(idx), name, place, tag, how, met);
                    }
                }
            }
        }
    }

    /**
     * A chain with one more copy at its end.
     *
     * @param chain The chain
     * @param num The copy
     * @return The longer chain
     */
    private static Ints longer(final Ints chain, final int num) {
        final Ints more = new Ints(chain.array());
        more.add(num);
        return more;
    }
}
