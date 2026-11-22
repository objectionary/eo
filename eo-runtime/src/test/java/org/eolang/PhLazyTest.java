/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhLazy}.
 *
 * @since 0.78.0
 */
final class PhLazyTest {

    @Test
    void doesNotBuildObjectBeforeItIsAskedFor() {
        final AtomicInteger built = new AtomicInteger();
        new PhLazy(
            () -> {
                built.incrementAndGet();
                return new PhDefault();
            }
        );
        MatcherAssert.assertThat(
            "PhLazy must not build its object until something asks for it, but it did",
            built.get(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void givesBytesOfObjectItBuilds() {
        final byte[] bytes = new byte[new SecureRandom().nextInt(1, 17)];
        new SecureRandom().nextBytes(bytes);
        MatcherAssert.assertThat(
            "PhLazy must dataize to the bytes of the object it builds, but it didnt",
            new Dataized(new PhLazy(() -> new PhDefault(bytes))).take(),
            Matchers.equalTo(bytes)
        );
    }

    @Test
    void keepsItsOwnTypeAfterCopy() {
        MatcherAssert.assertThat(
            "copy() must stay wrapped in PhLazy, but it didnt",
            new PhLazy(PhDefault::new).copy(),
            Matchers.instanceOf(PhLazy.class)
        );
    }
}
