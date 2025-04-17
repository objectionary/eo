/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Together;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ChCached}.
 *
 * @since 0.28.11
 */
final class ChCachedTest {

    @Test
    void cachesHashAndInvokesDelegateOnlyOnce() {
        final AtomicInteger invocations = new AtomicInteger(0);
        final ChCached cached = new ChCached(
            () -> {
                invocations.incrementAndGet();
                return "dummy";
            }
        );
        for (int idx = 0; idx < 10; ++idx) {
            MatcherAssert.assertThat(
                "The cached value should remain consistent across multiple calls",
                cached.value(),
                Matchers.equalTo("dummy")
            );
        }
        MatcherAssert.assertThat(
            "The delegate should be called exactly once, but it was not",
            invocations.get(),
            Matchers.equalTo(1)
        );
    }

    @RepeatedTest(10)
    void cachesHashInConcurrentEnvironment() {
        final AtomicInteger invocations = new AtomicInteger(0);
        final String tag = "parallel";
        final ChCached cached = new ChCached(
            () -> {
                invocations.incrementAndGet();
                return tag;
            }
        );
        MatcherAssert.assertThat(
            "We expect that all values are equal to the tag",
            new Together<>(30, i -> cached.value())
                .asList()
                .stream()
                .allMatch(tag::equals),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "The delegate should be called exactly once in concurrent environment, but it was not",
            invocations.get(),
            Matchers.equalTo(1)
        );
    }
}
