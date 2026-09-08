/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Agreed}.
 * @since 0.72.0
 */
final class AgreedTest {

    @Test
    void namesWhatEveryFillingCallsTheSame() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("Φ.oak", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        rows.put("Φ.elm", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        MatcherAssert.assertThat(
            "an oak and an elm both call their leaf green, but the dispatch was left unanswered",
            new Agreed(
                Arrays.asList(new Ref("Φ.oak"), new Ref("Φ.elm")),
                Arrays.asList("Φ.oak", "Φ.elm", "Φ.green"),
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                )
            ).members("Φ.grove.tree", Collections.singletonList("leaf")),
            Matchers.hasEntry("Φ.grove.tree.leaf", "Φ.green")
        );
    }

    @Test
    void namesNothingWhereTheFillingsCallItDifferently() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("Φ.oak", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        rows.put("Φ.elm", List.of(Map.of("name", "leaf", "type", "Φ.brown")));
        MatcherAssert.assertThat(
            "a green leaf and a brown one cannot both be the answer, but one was named",
            new Agreed(
                Arrays.asList(new Ref("Φ.oak"), new Ref("Φ.elm")),
                Arrays.asList("Φ.oak", "Φ.elm", "Φ.green", "Φ.brown"),
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                )
            ).members("Φ.grove.tree", Collections.singletonList("leaf")),
            Matchers.anEmptyMap()
        );
    }

    @Test
    void ignoresAFillingThatCannotAnswer() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("Φ.oak", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        rows.put("Φ.rock", List.of(Map.of("name", "weight", "type", "Φ.number")));
        MatcherAssert.assertThat(
            "a rock has no leaf to disagree about, but its silence was counted against the oak",
            new Agreed(
                Arrays.asList(new Ref("Φ.oak"), new Ref("Φ.rock")),
                Arrays.asList("Φ.oak", "Φ.rock", "Φ.green"),
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                )
            ).members("Φ.grove.tree", Collections.singletonList("leaf")),
            Matchers.hasEntry("Φ.grove.tree.leaf", "Φ.green")
        );
    }

    @Test
    void ignoresAFillingThatAnswersWithAnInventedName() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("Φ.oak", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        rows.put("Φ.vine", List.of(Map.of("void", "true", "name", "φ", "type", "Φ.vine.host")));
        MatcherAssert.assertThat(
            "the leaf of a vine is a name nobody wrote down, but it outvoted the oak",
            new Agreed(
                Arrays.asList(new Ref("Φ.oak"), new Ref("Φ.vine")),
                Arrays.asList("Φ.oak", "Φ.vine", "Φ.green"),
                new Provided(
                    rows, Collections.emptyMap(),
                    Collections.singletonList("Φ.vine.host"), Collections.emptyMap()
                )
            ).members("Φ.grove.tree", Collections.singletonList("leaf")),
            Matchers.hasEntry("Φ.grove.tree.leaf", "Φ.green")
        );
    }

    @Test
    void namesNothingWhereAFillingHasNoRowToRead() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("Φ.oak", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        MatcherAssert.assertThat(
            "a filling nobody wrote a row for cannot be asked anything, but it was believed",
            new Agreed(
                Collections.singletonList(new Ref("Φ.moss")),
                Arrays.asList("Φ.oak", "Φ.green"),
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                )
            ).members("Φ.grove.tree", Collections.singletonList("leaf")),
            Matchers.anEmptyMap()
        );
    }

    @Test
    void namesNothingWhereNobodyAsksAnything() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("Φ.oak", List.of(Map.of("name", "leaf", "type", "Φ.green")));
        MatcherAssert.assertThat(
            "a void no dispatch is rooted at has nothing to answer, but an answer appeared",
            new Agreed(
                Collections.singletonList(new Ref("Φ.oak")),
                Arrays.asList("Φ.oak", "Φ.green"),
                new Provided(
                    rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
                )
            ).members("Φ.grove.tree", Collections.emptyList()),
            Matchers.anEmptyMap()
        );
    }
}
