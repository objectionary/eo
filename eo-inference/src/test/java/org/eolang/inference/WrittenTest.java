/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Written}.
 *
 * @since 0.76.0
 */
final class WrittenTest {

    @Test
    void countsTheReceiversItsLocatorStepsInto() {
        MatcherAssert.assertThat(
            "three steps into a receiver must count three, but they didnt",
            new Written("Φ.jar.φ.ρ.ρ.α0.ρ", 4, "", ".lid").hops(),
            Matchers.equalTo(3)
        );
    }

    @Test
    void knowsItIsWhatAReadIsTakenOff() {
        MatcherAssert.assertThat(
            "the receiver of a read must know it is one, but it didnt",
            new Written("Φ.jar.φ.ρ", 2, "", ".lid").receives(
                new Written("Φ.jar.φ", 2, "", ".open")
            ),
            Matchers.is(true)
        );
    }

    @Test
    void isSpelledByItsBaseFirst() {
        MatcherAssert.assertThat(
            "an object whose base is the word must be the closest to it, but it wasnt",
            new Written("Φ.jar.φ", 2, "lid", "..open").spelling("open"),
            Matchers.equalTo(0)
        );
    }

    @Test
    void isSpelledByItsNameNext() {
        MatcherAssert.assertThat(
            "an object whose name is the word must come second, but it didnt",
            new Written("Φ.jar.lid", 2, "lid", "Φ.cap").spelling("lid"),
            Matchers.equalTo(1)
        );
    }

    @Test
    void countsTheDotsOfItsLocator() {
        MatcherAssert.assertThat(
            "the depth of a locator must be its dots, but it wasnt",
            new Written("Φ.jar.a🌵3-7.α1", 9, "", "").depth(),
            Matchers.equalTo(3)
        );
    }
}
