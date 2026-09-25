/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Optional;
import java.util.function.Supplier;
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
            "Wrapper without explicit term must delegate φ-term to the wrapped object, but it didnt",
            new PhOnceTest.Fake(() -> new PhDefault(new byte[] {(byte) 0x01})).φTerm(),
            Matchers.equalTo("[D> 01-]")
        );
    }

    @Test
    void keepsItsOwnTypeAfterNormalized() {
        MatcherAssert.assertThat(
            "normalized() must stay wrapped in the very type it started from, but it didnt",
            new PhOnceTest.Fake(() -> new PhDefault()).normalized(),
            Matchers.instanceOf(PhOnceTest.Fake.class)
        );
    }

    @Test
    void letsANormalizedTerminatorPropagateBare() {
        MatcherAssert.assertThat(
            "normalized() must not re-wrap a terminator, so callers can still detect it with instanceof",
            new PhOnceTest.Fake(PhTerminator::new).normalized(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void doesNotNeedRhoWithoutEvaluatingWrappedObject() {
        MatcherAssert.assertThat(
            "PhOnce must never ask for a receiver, and must not evaluate itself to say so",
            new PhOnceTest.Fake(
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
            "Wrapper with explicit term must render it without evaluating the wrapped object, but it didnt",
            new PhOnceTest.Fake(
                () -> {
                    throw new IllegalStateException("must not be evaluated");
                },
                Optional.of(() -> "x.foo")
            ).φTerm(),
            Matchers.equalTo("x.foo")
        );
    }

    @Test
    void keepsItsOwnTypeAfterCopy() {
        MatcherAssert.assertThat(
            "copy() must stay wrapped in the very type it started from, but it didnt",
            new PhOnceTest.Fake(PhDefault::new).copy(),
            Matchers.instanceOf(PhOnceTest.Fake.class)
        );
    }

    /**
     * A wrapper made concrete, since {@link PhOnce} is abstract.
     *
     * @since 0.60
     */
    private static final class Fake extends PhOnce {

        Fake(final Supplier<Phi> obj) {
            this(obj, Optional.empty());
        }

        Fake(final Supplier<Phi> obj, final Optional<Supplier<String>> phrase) {
            super(obj, phrase);
        }

        @Override
        public Phi wrapped(final Supplier<Phi> obj, final Optional<Supplier<String>> phrase) {
            return new PhOnceTest.Fake(obj, phrase);
        }
    }
}
