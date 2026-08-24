/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link PhSticky}.
 * @since 0.75
 */
final class PhStickyTest {

    @Test
    void computesThroughTheDecorator() {
        MatcherAssert.assertThat(
            "the decorated formation must dataize to the doubled input, but it didnt",
            new Dataized(
                new PhApplication(
                    new PhSticky(PhStickyTest.doubler(new AtomicInteger())),
                    new Bind(0, new Data.ToPhi(21.0d))
                )
            ).asNumber(),
            Matchers.equalTo(42.0d)
        );
    }

    @Test
    void dataizesBodyOnceForEqualInputs() {
        final AtomicInteger count = new AtomicInteger();
        final Phi twice = new PhSticky(PhStickyTest.doubler(count));
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(21.0d)))).take();
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(21.0d)))).take();
        MatcherAssert.assertThat(
            "the body must not be recomputed for an input already seen, but it was",
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void answersTheSameBytesFromTheCache() {
        final Phi twice = new PhSticky(PhStickyTest.doubler(new AtomicInteger()));
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(21.0d)))).take();
        MatcherAssert.assertThat(
            "the cached answer must equal the computed one, but it didnt",
            new Dataized(
                new PhApplication(twice, new Bind(0, new Data.ToPhi(21.0d)))
            ).asNumber(),
            Matchers.equalTo(42.0d)
        );
    }

    @Test
    void recomputesForDifferentInput() {
        final AtomicInteger count = new AtomicInteger();
        final Phi twice = new PhSticky(PhStickyTest.doubler(count));
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(21.0d)))).take();
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(7.0d)))).take();
        MatcherAssert.assertThat(
            "a fresh input must reach the body, but it didnt",
            count.get(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void treatsStringInputAsData() {
        final AtomicInteger count = new AtomicInteger();
        final Phi length = new PhSticky(PhStickyTest.measurer(count));
        new Dataized(new PhApplication(length, new Bind(0, new Data.ToPhi("kettle")))).take();
        new Dataized(new PhApplication(length, new Bind(0, new Data.ToPhi("kettle")))).take();
        MatcherAssert.assertThat(
            "a string input must hit the cache the second time, but it didnt",
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void bypassesCacheForNonDataInput() {
        final AtomicInteger count = new AtomicInteger();
        final Phi length = new PhSticky(PhStickyTest.measurer(count));
        new Dataized(
            new PhApplication(length, new Bind(0, new PhDefault(new byte[] {(byte) 0x2A})))
        ).take();
        new Dataized(
            new PhApplication(length, new Bind(0, new PhDefault(new byte[] {(byte) 0x2A})))
        ).take();
        MatcherAssert.assertThat(
            "an input that is not a number or a string must not be cached, but it was",
            count.get(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void cachesWhenReceiverIsANumber() {
        final AtomicInteger count = new AtomicInteger();
        final Phi halver = new PhSticky(PhStickyTest.receiver(count));
        final Phi first = halver.copy();
        first.put(Phi.RHO, new Data.ToPhi(84.0d));
        new Dataized(first).take();
        final Phi second = halver.copy();
        second.put(Phi.RHO, new Data.ToPhi(84.0d));
        new Dataized(second).take();
        MatcherAssert.assertThat(
            "a receiver put as a number must be an input like any other, but it wasnt cached",
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void asksForReceiverWhenOriginDeclaresOne() {
        MatcherAssert.assertThat(
            "the decorator must not hide the origin's appetite for a receiver, but it did",
            new PhSticky(PhStickyTest.receiver(new AtomicInteger())).needsRho(),
            Matchers.is(true)
        );
    }

    @Test
    void evictsOldestEntryBeyondCapacity() {
        final AtomicInteger count = new AtomicInteger();
        final Phi twice = new PhSticky(PhStickyTest.doubler(count), 2);
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(1.0d)))).take();
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(2.0d)))).take();
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(3.0d)))).take();
        new Dataized(new PhApplication(twice, new Bind(0, new Data.ToPhi(1.0d)))).take();
        MatcherAssert.assertThat(
            "the entry evicted by the capacity bound must be recomputed, but it wasnt",
            count.get(),
            Matchers.equalTo(4)
        );
    }

    @Test
    @Timeout(20L)
    void staysCorrectUnderConcurrentDataization() throws Exception {
        final Phi twice = new PhSticky(PhStickyTest.doubler(new AtomicInteger()));
        final List<Double> results = Collections.synchronizedList(new ArrayList<>(8));
        final Collection<Thread> threads = new ArrayList<>(8);
        for (int idx = 0; idx < 8; ++idx) {
            threads.add(
                new Thread(
                    () -> results.add(
                        new Dataized(
                            new PhApplication(twice, new Bind(0, new Data.ToPhi(21.0d)))
                        ).asNumber()
                    )
                )
            );
        }
        for (final Thread thread : threads) {
            thread.start();
        }
        for (final Thread thread : threads) {
            thread.join(10_000L);
        }
        MatcherAssert.assertThat(
            "every concurrent dataization must answer the same doubled input, but some didnt",
            results,
            Matchers.equalTo(Collections.nCopies(8, 42.0d))
        );
    }

    private static Phi doubler(final AtomicInteger count) {
        final PhDefault twice = new PhDefault();
        twice.add("x", new AtVoid("x"));
        twice.add(
            Phi.PHI,
            new AtOnce(
                new AtComposite(
                    twice,
                    self -> {
                        count.incrementAndGet();
                        return new Data.ToPhi(
                            new Dataized(self.take("x")).asNumber() * 2.0d
                        );
                    }
                )
            )
        );
        return twice;
    }

    private static Phi measurer(final AtomicInteger count) {
        final PhDefault length = new PhDefault();
        length.add("x", new AtVoid("x"));
        length.add(
            Phi.PHI,
            new AtOnce(
                new AtComposite(
                    length,
                    self -> {
                        count.incrementAndGet();
                        return new Data.ToPhi(
                            new Dataized(self.take("x")).take().length
                        );
                    }
                )
            )
        );
        return length;
    }

    private static Phi receiver(final AtomicInteger count) {
        final PhDefault halver = new PhDefault();
        halver.add(Phi.RHO, new AtRho());
        halver.add(
            Phi.PHI,
            new AtOnce(
                new AtComposite(
                    halver,
                    self -> {
                        count.incrementAndGet();
                        return new Data.ToPhi(
                            new Dataized(self.take(Phi.RHO)).asNumber() / 2.0d
                        );
                    }
                )
            )
        );
        return halver;
    }
}
