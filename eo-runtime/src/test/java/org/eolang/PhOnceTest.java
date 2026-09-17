/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhOnce}.
 *
 * @since 0.60
 */
final class PhOnceTest {

    @Test
    void delegatesTermToWrappedObjectByDefault() {
        MatcherAssert.assertThat(
            "PhOnce without explicit term must delegate φ-term to the wrapped object, but it didnt",
            new PhOnce(() -> new PhDefault(new byte[] {(byte) 0x01})).φTerm(),
            Matchers.equalTo("[D> 01-]")
        );
    }

    @Test
    void keepsOnceWrapperAfterNormalized() {
        MatcherAssert.assertThat(
            "normalized() must remain wrapped in PhOnce, so the once-caching guarantee survives, but it didn't",
            new PhOnce(() -> new PhDefault()).normalized(),
            Matchers.instanceOf(PhOnce.class)
        );
    }

    @Test
    void letsANormalizedTerminatorPropagateBare() {
        MatcherAssert.assertThat(
            "normalized() must not re-wrap a terminator, so callers can still detect it with instanceof",
            new PhOnce(PhTerminator::new).normalized(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void doesNotNeedRhoWithoutEvaluatingWrappedObject() {
        MatcherAssert.assertThat(
            "PhOnce must never ask for a receiver, and must not evaluate itself to say so",
            new PhOnce(
                () -> {
                    throw new IllegalStateException("must not be evaluated");
                }
            ).needsRho(),
            Matchers.is(false)
        );
    }

    @Test
    void doesNotEvaluateWrappedObjectForTerm() {
        MatcherAssert.assertThat(
            "PhOnce with explicit term must render it without evaluating the wrapped object, but it didnt",
            new PhOnce(
                () -> {
                    throw new IllegalStateException("must not be evaluated");
                },
                () -> "x.foo"
            ).φTerm(),
            Matchers.equalTo("x.foo")
        );
    }
}
