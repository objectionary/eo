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
            ).of("Φ.grove.α0", 0).where(),
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
            ).of("Φ.inc", 0).rung(),
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
            ).of("Φ.nowhere", 0).where(),
            Matchers.equalTo("Φ.nowhere")
        );
    }
}
