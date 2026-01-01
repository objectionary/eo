/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtOnce}.
 * @since 0.59.0
 */
final class AtOnceTest {
    @Test
    void throwsOnPut() {
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> new AtOnce(new AtVoid("void")).put(new PhDefault()),
            "AtOnce must throw an exception on put() operation"
        );
    }

    @Test
    void cachesAttribute() {
        final AtomicInteger count = new AtomicInteger();
        final Attr attr = new AtOnce(
            new AtComposite(
                new PhDefault(),
                phi -> {
                    count.incrementAndGet();
                    return phi;
                }
            )
        );
        attr.get();
        attr.get();
        MatcherAssert.assertThat(
            "AtOnce must execute nested attribute only once",
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void resetsCacheOnCopy() {
        final AtomicInteger count = new AtomicInteger();
        final Attr attr = new AtOnce(
            new AtComposite(
                new PhDefault(),
                phi -> {
                    count.incrementAndGet();
                    return phi;
                }
            )
        );
        attr.get();
        attr.copy(new PhDefault()).get();
        MatcherAssert.assertThat(
            "AtOnce must reset cache on copying",
            count.get(),
            Matchers.equalTo(2)
        );
    }
}
