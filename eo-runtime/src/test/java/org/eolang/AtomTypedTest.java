/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtomTyped}.
 * @since 0.57
 */
final class AtomTypedTest {

    @Test
    void throwsWhenComputedTypeDiffersFromDeclared() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new AtomTyped(
                new AtomTypedTest.Computing(new Data.ToPhi(42L)), "Φ.string"
            ).lambda(),
            "Atom computing a number while declaring a string must throw, but it didnt"
        );
    }

    @Test
    void returnsComputedObjectWhenTypeMatches() {
        final Phi number = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            "Typed atom must return its computed object when the declared type matches, but it didnt",
            new AtomTyped(new AtomTypedTest.Computing(number), "Φ.number").lambda(),
            Matchers.is(number)
        );
    }

    @Test
    void skipsCheckWhenDeclaredTypeIsEmpty() {
        final Phi number = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            "Typed atom with no declared type must return the computed object unchecked, but it didnt",
            new AtomTyped(new AtomTypedTest.Computing(number), "").lambda(),
            Matchers.is(number)
        );
    }

    /**
     * An atom that computes a fixed object.
     * @since 0.57
     */
    private static final class Computing extends PhDefault implements Atom {

        /**
         * The object to compute.
         */
        private final Phi result;

        /**
         * Ctor.
         * @param phi The object to compute
         */
        Computing(final Phi phi) {
            this.result = phi;
        }

        @Override
        public Phi lambda() {
            return this.result;
        }
    }
}
