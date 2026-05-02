/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.scalar.ScalarOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
    @ExtendWith(WeAreOnline.class)
    void downloadsAndChecksFromRealSource() throws Exception {
        MatcherAssert.assertThat(
            "The index must contain the default value",
            new ObjectsIndex().contains("io.stdout"),
            Matchers.is(true)
        );
    }
}
