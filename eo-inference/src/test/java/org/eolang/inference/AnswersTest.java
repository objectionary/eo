/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Answers}.
 * @since 0.70.0
 */
final class AnswersTest {

    @Test
    void namesTheFormationAnObjectSettledOn() {
        final Map<String, Collection<Map<String, String>>> rows = new LinkedHashMap<>(0);
        final Map<String, String> whole = new LinkedHashMap<>(0);
        whole.put("id", "Φ.oak");
        whole.put("complete", "true");
        rows.put("Φ.oak", Collections.singletonList(whole));
        final Map<String, String> chain = new LinkedHashMap<>(0);
        chain.put("Φ.grove.α0", "Φ.oak");
        MatcherAssert.assertThat(
            "a copy of an oak must answer that it is an oak, but it named something else",
            new Answers(
                rows, Collections.emptyMap(), Collections.emptyList(), chain
            ).of("Φ.grove.α0", Collections.emptyList()).where(),
            Matchers.equalTo("Φ.oak")
        );
    }

    @Test
    void keepsAFreeVoidOffTheTopRung() {
        final Map<String, Collection<Map<String, String>>> rows = new LinkedHashMap<>(0);
        final Map<String, String> whole = new LinkedHashMap<>(0);
        whole.put("id", "Φ.inc");
        whole.put("complete", "true");
        final Map<String, String> hollow = new LinkedHashMap<>(0);
        hollow.put("owner", "Φ.inc");
        hollow.put("name", "x");
        hollow.put("void", "true");
        rows.put("Φ.inc", Arrays.asList(whole, hollow));
        MatcherAssert.assertThat(
            "an inc whose void nobody filled cannot be known whole, but it was",
            new Answers(
                rows, Collections.emptyMap(), Collections.emptyList(),
                Collections.emptyMap()
            ).of("Φ.inc", Collections.emptyList()).rung(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void answersWithTheObjectItselfWhenNothingIsKnown() {
        MatcherAssert.assertThat(
            "an object no table mentions must answer with itself, but it named something else",
            new Answers(
                Collections.emptyMap(), Collections.emptyMap(),
                Collections.emptyList(), Collections.emptyMap()
            ).of("Φ.nowhere", Collections.emptyList()).where(),
            Matchers.equalTo("Φ.nowhere")
        );
    }

    @Test
    void countsTheVoidsFilledEarlierInTheChain() {
        final Map<String, Collection<Map<String, String>>> rows = new LinkedHashMap<>(0);
        final Map<String, String> whole = new LinkedHashMap<>(0);
        whole.put("id", "Φ.pair");
        whole.put("complete", "true");
        final Map<String, String> left = new LinkedHashMap<>(0);
        left.put("name", "x");
        left.put("void", "true");
        left.put("type", "Φ.pair.x");
        final Map<String, String> right = new LinkedHashMap<>(0);
        right.put("name", "y");
        right.put("void", "true");
        right.put("type", "Φ.pair.y");
        rows.put("Φ.pair", Arrays.asList(whole, left, right));
        MatcherAssert.assertThat(
            "a copy whose voids were filled one at a time has none left free, but it had",
            new Answers(
                rows, Collections.emptyMap(), Collections.emptyList(),
                Collections.singletonMap("Φ.app.full", "Φ.pair")
            ).of("Φ.app.full", Arrays.asList("Φ.pair.x", "Φ.pair.y")).rung(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void answersAVoidWithTheOneTypeThatFillsIt() {
        MatcherAssert.assertThat(
            "a void the whole program fills with a tuple must answer that it is one, but it didnt",
            new Answers(
                AnswersTest.formation("Φ.tuple"),
                Collections.singletonMap(
                    "Φ.printf.args", Collections.singletonList(new Ref("Φ.tuple"))
                ),
                Collections.emptyList(),
                Collections.emptyMap()
            ).of("Φ.printf.args", Collections.emptyList()).where(),
            Matchers.equalTo("Φ.tuple")
        );
    }

    @Test
    void takesTheRungOfTheTypeThatFillsTheVoid() {
        MatcherAssert.assertThat(
            "a void filled with a whole formation must stand where it stands, but it stayed below",
            new Answers(
                AnswersTest.formation("Φ.tuple"),
                Collections.singletonMap(
                    "Φ.printf.args", Collections.singletonList(new Ref("Φ.tuple"))
                ),
                Collections.emptyList(),
                Collections.emptyMap()
            ).of("Φ.printf.args", Collections.emptyList()).rung(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void leavesAVoidSeveralTypesFillAlone() {
        MatcherAssert.assertThat(
            "a void two formas fill cannot be either of them, but it was named one",
            new Answers(
                AnswersTest.formation("Φ.tuple"),
                Collections.singletonMap(
                    "Φ.i8.as-bytes",
                    Arrays.asList(new Ref("Φ.tuple"), new Ref("Φ.string"))
                ),
                Collections.emptyList(),
                Collections.emptyMap()
            ).of("Φ.i8.as-bytes", Collections.emptyList()).where(),
            Matchers.equalTo("Φ.i8.as-bytes")
        );
    }

    @Test
    void leavesAVoidFilledFromAnotherVoidAlone() {
        MatcherAssert.assertThat(
            "a void filled from a void names no forma, so it must stay itself, but it moved",
            new Answers(
                AnswersTest.formation("Φ.tuple"),
                Collections.singletonMap(
                    "Φ.one.x", Collections.singletonList(new Var("Φ.two.y"))
                ),
                Collections.emptyList(),
                Collections.emptyMap()
            ).of("Φ.one.x", Collections.emptyList()).where(),
            Matchers.equalTo("Φ.one.x")
        );
    }

    @Test
    void doesNotCountTheReceiverAmongTheVoidsFilled() {
        final Map<String, Collection<Map<String, String>>> rows = new LinkedHashMap<>(0);
        final Map<String, String> whole = new LinkedHashMap<>(0);
        whole.put("id", "Φ.grow");
        whole.put("complete", "true");
        final Map<String, String> bearer = new LinkedHashMap<>(0);
        bearer.put("name", "ρ");
        bearer.put("void", "true");
        bearer.put("type", "Φ.grow.ρ");
        final Map<String, String> hollow = new LinkedHashMap<>(0);
        hollow.put("name", "x");
        hollow.put("void", "true");
        hollow.put("type", "Φ.grow.x");
        rows.put("Φ.grow", Arrays.asList(whole, bearer, hollow));
        MatcherAssert.assertThat(
            "a dispatch fills the receiver and nothing else, so x must stay free, but it didnt",
            new Answers(
                rows, Collections.emptyMap(), Collections.emptyList(),
                Collections.singletonMap("Φ.app.half", "Φ.grow")
            ).of("Φ.app.half", Collections.singletonList("Φ.grow.ρ")).rung(),
            Matchers.equalTo(2)
        );
    }

    private static Map<String, Collection<Map<String, String>>> formation(final String locator) {
        final Map<String, String> whole = new LinkedHashMap<>(0);
        whole.put("id", locator);
        whole.put("complete", "true");
        return Collections.singletonMap(locator, Collections.singletonList(whole));
    }
}
