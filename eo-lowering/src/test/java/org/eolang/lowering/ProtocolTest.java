/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Protocol}.
 *
 * @since 0.76.0
 */
final class ProtocolTest {

    @Test
    void answersWithGivenKey() {
        MatcherAssert.assertThat(
            "the answer must come back as given, but it didnt",
            new Protocol(Collections.emptyList(), "sym:v0", "bytes").answer(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void repeatsWhenAnArmDoes() {
        MatcherAssert.assertThat(
            "a program whose fork repeats in one arm must repeat, but it doesnt",
            new Protocol(
                Collections.singletonList(
                    new Fork(
                        "s1", "L_bool_if", "sym:v0",
                        new Protocol(Collections.emptyList(), Collections.singletonList("sym:v0")),
                        new Protocol(Collections.emptyList(), "sym:v0", "bool")
                    )
                ),
                "sym:s1",
                "bool"
            ).repeats(),
            Matchers.is(true)
        );
    }

    @Test
    void answersNothingWhenRepeating() {
        MatcherAssert.assertThat(
            "a program that repeats must name no carrier, but it does",
            new Protocol(
                Collections.emptyList(), Collections.singletonList("sym:v0")
            ).carrier(),
            Matchers.emptyString()
        );
    }

    @Test
    void namesCarrier() {
        MatcherAssert.assertThat(
            "the forma must come back as given, but it didnt",
            new Protocol(Collections.emptyList(), "bool:FF-", "bool").carrier(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void failsWithGivenReason() {
        MatcherAssert.assertThat(
            "the reason must come back as given, but it didnt",
            new Protocol(Collections.emptyList(), "sym:v0").reason(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void answersNothingWhenFailing() {
        MatcherAssert.assertThat(
            "a failing program has no answer and no carrier, but it named one",
            String.join(
                "",
                new Protocol(Collections.emptyList(), "sym:v0").answer(),
                new Protocol(Collections.emptyList(), "sym:v0").carrier()
            ),
            Matchers.emptyString()
        );
    }

    @Test
    void doesNotRepeatWhenFailing() {
        MatcherAssert.assertThat(
            "a failing program needs no loop, but it asks for one",
            new Protocol(Collections.emptyList(), "sym:v0").repeats(),
            Matchers.is(false)
        );
    }

    @Test
    void failsWithNothingWhenAnswering() {
        MatcherAssert.assertThat(
            "an answering program has no reason to fail, but it names one",
            new Protocol(Collections.emptyList(), "sym:v0", "bytes").reason(),
            Matchers.emptyString()
        );
    }
}
