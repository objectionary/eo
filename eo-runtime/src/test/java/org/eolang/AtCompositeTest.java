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
 * Test case for {@link AtComposite}.
 * @since 0.59.0
 */
final class AtCompositeTest {
    @Test
    void throwsOnPut() {
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> new AtComposite(new PhDefault(), phi -> phi).put(new PhDefault()),
            "AtComposite must throw an exception on put() operation"
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void executesExpression() {
        final AtomicInteger count = new AtomicInteger();
        new AtComposite(
            new PhDefault(),
            phi -> {
                count.incrementAndGet();
                return phi;
            }
        ).get();
        MatcherAssert.assertThat(
            "AtComposite must execute given expression",
            count.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void returnsValidObject() {
        final Phi phi = new PhDefault();
        MatcherAssert.assertThat(
            "AtComposite must return valid object",
            new AtComposite(phi, obj -> obj).get(),
            Matchers.equalTo(phi)
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void changesArgumentOnCopying() {
        final Phi first = new PhDefault();
        final Attr attr = new AtComposite(first, phi -> phi);
        final Phi res = attr.get();
        final Phi copy = attr.copy(new PhDefault()).get();
        MatcherAssert.assertThat(
            "AtComposite must change expression argument on copying",
            res,
            Matchers.allOf(
                Matchers.equalTo(first),
                Matchers.not(Matchers.equalTo(copy))
            )
        );
    }
}
