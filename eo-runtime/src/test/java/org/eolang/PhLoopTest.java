/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link PhLoop}.
 *
 * @since 0.76
 */
final class PhLoopTest {

    @Test
    void answersTheValueOfTheLastCall() {
        MatcherAssert.assertThat(
            "the loop must dataize to what the last tail call answers, but it didnt",
            new Dataized(PhLoopTest.countdown(7L, new AtomicInteger())).asNumber(),
            Matchers.equalTo(42.0d)
        );
    }

    @Test
    @Timeout(120L)
    void runsManyTailCallsOnASmallStack() throws InterruptedException {
        final AtomicReference<Double> result = new AtomicReference<>();
        final Thread thread = new Thread(
            null,
            () -> result.set(
                new Dataized(
                    PhLoopTest.countdown(20_000L, new AtomicInteger())
                ).asNumber()
            ),
            "loop",
            256L << 10
        );
        thread.start();
        thread.join(100_000L);
        MatcherAssert.assertThat(
            "twenty thousand tail calls must fit into a small stack, but they didnt",
            result.get(),
            Matchers.equalTo(42.0d)
        );
    }

    @Test
    void answersOwnAttributeWithoutForcingTheBody() {
        MatcherAssert.assertThat(
            "an attribute of the formation itself must not be looked up through the chain, but it was",
            new Dataized(
                PhLoopTest.countdown(9L, new AtomicInteger()).take("left")
            ).asNumber(),
            Matchers.equalTo(9.0d)
        );
    }

    @Test
    void looksUpAnAbsentAttributeThroughTheChain() {
        MatcherAssert.assertThat(
            "an attribute the formation lacks must be found on the answer of the last call, but it wasnt",
            new Dataized(
                new PhApplication(
                    PhLoopTest.countdown(4L, new AtomicInteger()).take("plus"),
                    new Bind(0, new Data.ToPhi(8.0d))
                )
            ).asNumber(),
            Matchers.equalTo(50.0d)
        );
    }

    @Test
    void normalizesThroughTheChain() {
        MatcherAssert.assertThat(
            "the normal form must be the one of the last call, but it wasnt",
            new Dataized(
                PhLoopTest.countdown(6L, new AtomicInteger()).normalized()
            ).asNumber(),
            Matchers.equalTo(42.0d)
        );
    }

    @Test
    void walksTheChainOnce() {
        final AtomicInteger taken = new AtomicInteger();
        final Phi loop = PhLoopTest.countdown(10L, taken);
        new Dataized(loop).take();
        new Dataized(loop).take();
        MatcherAssert.assertThat(
            "a second dataization must jump to the body that completed instead of walking the chain again, but it walked",
            taken.get(),
            Matchers.equalTo(12)
        );
    }

    private static Phi countdown(final long from, final AtomicInteger taken) {
        final PhDefault template = new PhDefault() {
            @Override
            public Phi take(final String name) {
                if (Phi.PHI.equals(name)) {
                    taken.incrementAndGet();
                }
                return super.take(name);
            }
        };
        template.add("left", new AtVoid("left"));
        template.add(
            Phi.PHI,
            new AtOnce(
                new AtComposite(
                    template,
                    self -> {
                        final double left = new Dataized(self.take("left")).asNumber();
                        final Phi body;
                        if (left == 0.0d) {
                            body = new Data.ToPhi(42.0d);
                        } else {
                            final Phi next = template.copy();
                            next.put("left", new Data.ToPhi(left - 1.0d));
                            body = new PhAgain(next);
                        }
                        return body;
                    }
                )
            )
        );
        final Phi count = template.copy();
        count.put("left", new Data.ToPhi((double) from));
        return new PhLoop(count);
    }
}
