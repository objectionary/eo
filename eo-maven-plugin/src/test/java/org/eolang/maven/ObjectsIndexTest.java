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
 *
 * @since 0.29
 */
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class ObjectsIndexTest {

    @Test
    void containsKnownObject() throws Exception {
        final String object = "org.eolang.io.stderr";
        MatcherAssert.assertThat(
            "The index must contain the known value",
            new ObjectsIndex(
                new ScalarOf<>(() -> Collections.singleton(object))
            ).contains(object),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotContainUnknownObject() throws Exception {
        MatcherAssert.assertThat(
            "The index must not contain the unknown value",
            new ObjectsIndex(
                new ScalarOf<>(() -> Collections.singleton("org.eolang.io.stderr"))
            ).contains("unknown"),
            Matchers.is(false)
        );
    }

    @Test
    void cachesResultAndCallsScalarOnce() throws Exception {
        final AtomicInteger calls = new AtomicInteger(0);
        final String object = "org.eolang.io.stderr";
        final ObjectsIndex index = new ObjectsIndex(
            new ScalarOf<>(
                () -> {
                    calls.incrementAndGet();
                    return Collections.singleton(object);
                }
            )
        );
        index.contains(object);
        index.contains(object);
        MatcherAssert.assertThat(
            "The scalar should be called exactly once due to caching",
            calls.get(),
            Matchers.is(1)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void downloadsAndChecksFromRealSource() throws Exception {
        MatcherAssert.assertThat(
            "The index must contain the default value",
            new ObjectsIndex().contains("org.eolang.io.stdout"),
            Matchers.is(true)
        );
    }
}
