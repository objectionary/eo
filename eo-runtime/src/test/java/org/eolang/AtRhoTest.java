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
 * @since 0.59.0
 */
final class AtRhoTest {

    @Test
    void printsQuestionMarkAsTermWhileUnset() {
        MatcherAssert.assertThat(
            "an unset rho must render as a question mark in φ-term, but it didnt",
            new AtRho().φTerm(),
            Matchers.equalTo("?")
        );
    }

    @Test
    void printsCaretAsTermOnceBound() {
        final Attribute rho = new AtRho();
        rho.put(new PhDefault());
        MatcherAssert.assertThat(
            "a bound rho must render as a caret in φ-term, but it didnt",
            rho.φTerm(),
            Matchers.equalTo("^")
        );
    }

    @Test
    void terminatesOnEmptyRho() {
        MatcherAssert.assertThat(
            "reading an unset rho must give a termination, but it didnt",
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
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> rho.put(new PhDefault()),
            "a rho that is already bound must reject a second put"
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

    @Test
    void reportsItselfVacantWhileUnset() {
        MatcherAssert.assertThat(
            "an unset rho attribute must report itself vacant, but it didnt",
            new AtRho().vacant(),
            Matchers.is(true)
        );
    }

    @Test
    void stopsBeingVacantOnceBound() {
        final AtRho attr = new AtRho();
        attr.put(new PhDefault());
        MatcherAssert.assertThat(
            "a bound rho attribute must stop reporting itself vacant, but it didnt",
            attr.vacant(),
            Matchers.is(false)
        );
    }
}
