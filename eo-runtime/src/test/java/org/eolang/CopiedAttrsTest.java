/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link CopiedAttrs}.
 * @since 0.63
 */
final class CopiedAttrsTest {

    @Test
    void copiesNoAttributeThatNobodyTakes() {
        final AtomicInteger copies = new AtomicInteger();
        new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy();
        MatcherAssert.assertThat(
            "copying an object must leave an attribute nobody takes alone, but it copied it",
            copies.get(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void copiesTheAttributeThatIsTaken() {
        final AtomicInteger copies = new AtomicInteger();
        new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy().take("x");
        MatcherAssert.assertThat(
            "taking an attribute out of a copy must copy it once, but it didnt",
            copies.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void copiesTheSameAttributeOnlyOnce() {
        final AtomicInteger copies = new AtomicInteger();
        final Phi copy = new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy();
        copy.take("x");
        copy.take("x");
        MatcherAssert.assertThat(
            "taking one attribute twice must copy it once, but it copied it again",
            copies.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    @SuppressWarnings("PMD.CloseResource")
    void copiesTheSameAttributeOnlyOnceUnderConcurrentTake() throws InterruptedException {
        final int threads = 64;
        final AtomicInteger copies = new AtomicInteger();
        final Phi copy = new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy();
        final CountDownLatch ready = new CountDownLatch(threads);
        final CountDownLatch start = new CountDownLatch(1);
        final CountDownLatch done = new CountDownLatch(threads);
        final ExecutorService pool = Executors.newFixedThreadPool(threads);
        try {
            for (int idx = 0; idx < threads; ++idx) {
                pool.execute(
                    () -> {
                        ready.countDown();
                        this.awaited(start);
                        copy.take("x");
                        done.countDown();
                    }
                );
            }
            ready.await();
            start.countDown();
            done.await(10, TimeUnit.SECONDS);
        } finally {
            pool.shutdownNow();
        }
        MatcherAssert.assertThat(
            "taking one attribute from many threads at once must copy it once, but it copied more",
            copies.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    @SuppressWarnings("PMD.CloseResource")
    void losesNoAttributeUnderConcurrentTakeOfDifferentNames()
        throws InterruptedException, ExecutionException, TimeoutException {
        final int threads = 64;
        final Phi copy = new PhDefault(
            new Attrs(
                IntStream.range(0, threads).mapToObj(
                    idx -> new Attr(
                        String.format("a%d", idx), new AtVoid(String.format("a%d", idx))
                    )
                ).toArray(Attr[]::new)
            )
        ).copy();
        final CountDownLatch ready = new CountDownLatch(threads);
        final CountDownLatch start = new CountDownLatch(1);
        final List<Boolean> found;
        final ExecutorService pool = Executors.newFixedThreadPool(threads);
        try {
            final List<Future<Boolean>> futures = IntStream.range(0, threads).mapToObj(
                idx -> pool.submit(
                    () -> {
                        ready.countDown();
                        this.awaited(start);
                        return copy.take(String.format("a%d", idx)) != null;
                    }
                )
            ).collect(Collectors.toList());
            ready.await();
            start.countDown();
            found = this.resolved(futures);
        } finally {
            pool.shutdownNow();
        }
        MatcherAssert.assertThat(
            "taking every attribute at once must find all of them, but some were lost",
            found,
            Matchers.everyItem(Matchers.is(true))
        );
    }

    /**
     * Wait on a latch, converting the checked interruption into an unchecked one.
     * @param latch The latch to wait on
     */
    private void awaited(final CountDownLatch latch) {
        try {
            latch.await();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Resolve every future, letting its checked exceptions propagate.
     * @param futures The futures to resolve
     * @return Their resolved values, in the same order
     * @throws ExecutionException If any future failed
     * @throws TimeoutException If any future did not finish in time
     * @throws InterruptedException If interrupted while waiting
     */
    private List<Boolean> resolved(final List<Future<Boolean>> futures)
        throws ExecutionException, TimeoutException, InterruptedException {
        final List<Boolean> values = new ArrayList<>(futures.size());
        for (final Future<Boolean> future : futures) {
            values.add(future.get(10, TimeUnit.SECONDS));
        }
        return values;
    }

    /**
     * Attribute that remembers how many times it was copied.
     * @since 0.63
     */
    private static final class AtCounting implements Attribute {

        /**
         * Where the copies are counted.
         */
        private final AtomicInteger copies;

        /**
         * Ctor.
         * @param count Where to count the copies
         */
        AtCounting(final AtomicInteger count) {
            this.copies = count;
        }

        @Override
        public Attribute copy(final Phi self) {
            this.copies.incrementAndGet();
            return this;
        }

        @Override
        public Phi get() {
            return new PhDefault();
        }

        @Override
        public void put(final Phi phi) {
            throw new UnsupportedOperationException("this attribute takes nothing");
        }

        @Override
        public boolean vacant() {
            return false;
        }

        @Override
        public String φTerm() {
            return "counting";
        }
    }
}
