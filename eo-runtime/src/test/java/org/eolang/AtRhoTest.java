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
 * Test case for {@link AtRho}.
 *
 * @since 0.59.0
 */
final class AtRhoTest {

    @Test
    void printsQuestionMarkAsTermWhenUnset() {
        MatcherAssert.assertThat(
            "AtRho must render as question mark in φ-term while unset, but it didnt",
            new AtRho().φTerm(),
            Matchers.equalTo("?")
        );
    }

    @Test
    void printsCaretAsTermWhenSet() {
        final Attribute rho = new AtRho();
        rho.put(new PhDefault());
        MatcherAssert.assertThat(
            "AtRho must render as caret in φ-term once set, but it didnt",
            rho.φTerm(),
            Matchers.equalTo("^")
        );
    }

    @Test
    void terminatesOnEmptyRho() {
        MatcherAssert.assertThat(
            "AtRho must terminate, not throw, while the attribute is not set",
            new AtRho().get(),
            Matchers.instanceOf(PhTerminator.class)
        );
    }

    @Test
    void returnsSetRho() {
        final Attribute rho = new AtRho();
        final Phi obj = new PhDefault();
        rho.put(obj);
        MatcherAssert.assertThat(
            "AtRho must successfully return set object",
            rho.get(),
            Matchers.is(obj)
        );
    }

    @Test
    void doesNotCopyObjectOnCopying() {
        final Attribute rho = new AtRho();
        final Phi obj = new PhDefault();
        rho.put(obj);
        MatcherAssert.assertThat(
            "AtRho must not copy inner object on copy() operation",
            rho.copy(new PhDefault()).get(),
            Matchers.is(obj)
        );
    }

    @Test
    void rejectsSecondPut() {
        final Attribute rho = new AtRho();
        rho.put(new PhDefault());
        MatcherAssert.assertThat(
            "AtRho must reject the second put instead of dropping it silently",
            Assertions.assertThrows(
                ExReadOnly.class,
                () -> rho.put(new PhDefault())
            ).getMessage(),
            Matchers.containsString("already set")
        );
    }

    @Test
    void rejectsNullValue() {
        MatcherAssert.assertThat(
            "AtRho must reject null instead of silently keeping rho unset",
            Assertions.assertThrows(
                NullPointerException.class,
                () -> new AtRho().put(null)
            ).getMessage(),
            Matchers.containsString("can't be null")
        );
    }
}
