/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtomType}.
 *
 * @since 0.76.0
 */
final class AtomTypeTest {

    @Test
    void admitsTheFormaItNames() {
        MatcherAssert.assertThat(
            "a type that names one forma must admit that forma, but it didnt",
            new AtomType("Φ.number").admits("Φ.number"),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsAnotherForma() {
        MatcherAssert.assertThat(
            "a type that names one forma must not admit another one, but it did",
            new AtomType("Φ.string").admits("Φ.number"),
            Matchers.is(false)
        );
    }

    @Test
    void admitsTheHeadOfTree() {
        MatcherAssert.assertThat(
            "a tree must admit the forma before its brackets, but it didnt",
            new AtomType("Φ.posix.status-return(Φ.number,Φ.string(Φ.bytes))")
                .admits("Φ.posix.status-return"),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsFormaInsideBracketsOfTree() {
        MatcherAssert.assertThat(
            "a tree must not admit a forma that only its brackets name, but it did",
            new AtomType("Φ.posix.status-return(Φ.number,Φ.string)").admits("Φ.number"),
            Matchers.is(false)
        );
    }

    @Test
    void admitsAnyFormaForTypeVariable() {
        MatcherAssert.assertThat(
            "a type variable must admit any forma, but it didnt",
            new AtomType("C").admits(
                String.format("Φ.q%d", new SecureRandom().nextInt(10_000))
            ),
            Matchers.is(true)
        );
    }

    @Test
    void admitsAnyFormaForEmptyType() {
        MatcherAssert.assertThat(
            "a type nobody declared must admit any forma, but it didnt",
            new AtomType("").admits(
                String.format("Φ.w%d", new SecureRandom().nextInt(10_000))
            ),
            Matchers.is(true)
        );
    }
}
