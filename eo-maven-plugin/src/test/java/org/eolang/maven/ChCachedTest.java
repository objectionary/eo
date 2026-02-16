/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Together;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ChCached}.
 *
 * @since 0.28.11
 */
@SuppressWarnings({"PMD.UnnecessaryLocalRule", "PMD.UnitTestContainsTooManyAsserts"})
final class ChCachedTest {

    @Test
    void raisesException() {
        MatcherAssert.assertThat(
            "The exception message should provide information about the inner problem",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new ChCached(
                    () -> {
                        throw new IllegalStateException("inner problem");
                    }
                ).value(),
                "An exception should be thrown, but it was not"
            ).getMessage(),
            Matchers.containsString("inner problem")
        );
    }

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
            cached.value();
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
        final ChCached cached = new ChCached(
            () -> {
                invocations.incrementAndGet();
                return "parallel";
            }
        );
        new Together<>(30, i -> cached.value()).asList();
        MatcherAssert.assertThat(
            "The delegate should be called exactly once in concurrent environment, but it was not",
            invocations.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void returnsNullIfDelegateReturnsNull() {
        MatcherAssert.assertThat(
            "The cached value should be null",
            new ChCached(() -> null).value(),
            Matchers.nullValue()
        );
    }
}
