/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.scalar.ScalarOf;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link ObjectsIndex}.
 * @since 0.29
 */
final class ObjectsIndexTest {

    @Test
    void runsContainsWithOnlyOneCallToDecoratedObject() throws Exception {
        final AtomicInteger calls = new AtomicInteger(0);
        final ObjectsIndex index = new ObjectsIndex(
            new ScalarOf<>(
                () -> {
                    calls.incrementAndGet();
                    return Collections.singleton("io.stderr");
                }
            )
        );
        index.contains("org.eolang.io.stderr");
        index.contains("org.eolang.io.stderr");
        MatcherAssert.assertThat(
            String.format(
                "Scalar was called %d times instead of exactly once",
                calls.get()
            ),
            calls.get(),
            Matchers.is(1)
        );
    }

    @RepeatedTest(20)
    void readsTheIndexOnceFromManyThreads() throws Exception {
        final AtomicInteger calls = new AtomicInteger(0);
        final ObjectsIndex index = new ObjectsIndex(
            new ScalarOf<>(
                () -> {
                    calls.incrementAndGet();
                    Thread.sleep(5L);
                    return Collections.singleton("io.stderr");
                }
            )
        );
        MatcherAssert.assertThat(
            "the index must be read once, however many workers ask it at once",
            ObjectsIndexTest.asked(index, 8),
            Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
            "the reads must be counted only once too",
            calls.get(),
            Matchers.equalTo(1)
        );
    }

    /**
     * Ask one index from many threads at once.
     * @param index The index to ask
     * @param workers How many threads ask it
     * @return How many distinct answers came back
     * @throws Exception If any of the workers fails
     */
    private static int asked(final ObjectsIndex index, final int workers) throws Exception {
        final CountDownLatch latch = new CountDownLatch(1);
        final ExecutorService pool = Executors.newFixedThreadPool(workers);
        final Collection<Boolean> answers = new HashSet<>(1);
        try {
            final List<Future<Boolean>> futures = new ArrayList<>(workers);
            for (int worker = 0; worker < workers; ++worker) {
                futures.add(
                    pool.submit(
                        () -> {
                            latch.await();
                            return index.contains("org.eolang.io.stderr");
                        }
                    )
                );
            }
            latch.countDown();
            for (final Future<Boolean> future : futures) {
                answers.add(future.get());
            }
        } finally {
            pool.shutdown();
        }
        return answers.size();
    }

    @Test
    void runsContainsSuccessfully() throws Exception {
        MatcherAssert.assertThat(
            "The object must contain the value",
            new ObjectsIndex(
                new ScalarOf<>(
                    () -> {
                        return Collections.singleton("io.stderr");
                    }
                )
            ).contains("org.eolang.io.stderr"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotContainUnknownValue() throws Exception {
        MatcherAssert.assertThat(
            "The index must not contain the unknown value",
            new ObjectsIndex(
                new ScalarOf<>(
                    () -> {
                        return Collections.singleton("io.stderr");
                    }
                )
            ).contains("unknown"),
            Matchers.is(false)
        );
    }

    @Test
    void listsDirectChildrenOfPackage() throws Exception {
        MatcherAssert.assertThat(
            "The index must list every object that lives directly in the package",
            new ObjectsIndex(
                new ScalarOf<>(
                    () -> new SetOf<>(
                        "tuple",
                        "tuple.each",
                        "tuple.eachi",
                        "tuple.inner.deep",
                        "math.abs"
                    )
                )
            ).children("tuple"),
            Matchers.containsInAnyOrder("tuple.each", "tuple.eachi")
        );
    }

    @Test
    void listsDirectChildrenOfPackageWithOrgEolangPrefix() throws Exception {
        MatcherAssert.assertThat(
            "children() must strip a leading org.eolang. package the same way contains() does",
            new ObjectsIndex(
                new ScalarOf<>(
                    () -> new SetOf<>(
                        "tuple",
                        "tuple.each",
                        "tuple.eachi",
                        "tuple.inner.deep",
                        "math.abs"
                    )
                )
            ).children("org.eolang.tuple"),
            Matchers.containsInAnyOrder("tuple.each", "tuple.eachi")
        );
    }

    @Test
    void listsDirectChildrenOfTheBareRootPackage() throws Exception {
        MatcherAssert.assertThat(
            "children() must strip a bare org.eolang with no trailing dot the same way it strips org.eolang.",
            new ObjectsIndex(
                new ScalarOf<>(
                    () -> new SetOf<>(
                        "tuple",
                        "tuple.each",
                        "math",
                        "math.abs"
                    )
                )
            ).children("org.eolang"),
            Matchers.containsInAnyOrder("tuple", "math")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void downloadsAndChecksFromRealSource() throws Exception {
        MatcherAssert.assertThat(
            "The index must contain the default value",
            new ObjectsIndex().contains("stdout"),
            Matchers.is(true)
        );
    }
}
