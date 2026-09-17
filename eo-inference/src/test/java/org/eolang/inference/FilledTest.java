/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Filled}.
 *
 * @since 0.69.0
 */
final class FilledTest {

    @Test
    void prefersAnExactMatchOverAPrefixMatch() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "form",
            List.of(
                Map.of("void", "true", "type", "Φ.node.x"),
                Map.of("void", "true", "type", "Φ.node")
            )
        );
        final Provided owned = new Provided(
            rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
        );
        final Map<String, String> pairs = Map.of("app", "form");
        final Map<String, Map<String, String>> bound = new Bound(
            Map.of("app", List.of("value-x", "value-foo")),
            Collections.emptyMap(), Collections.emptyMap(), pairs, owned
        ).all();
        MatcherAssert.assertThat(
            "an exact fill of the whole answer must win over a fill of one of its prefixes",
            new Filled(
                pairs,
                owned,
                new Puts(bound, new Holders(bound, pairs).all()),
                Collections.emptyList()
            ).instead("Φ.node.x", "app", "app"),
            Matchers.equalTo("value-x")
        );
    }

    @Test
    void prefersTheLongestOfTwoMatchingPrefixes() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "form",
            List.of(
                Map.of("void", "true", "type", "Φ.node"),
                Map.of("void", "true", "type", "Φ.node.x")
            )
        );
        rows.put("long-fill", List.of(Map.of("name", "y", "type", "Φ.result")));
        final Provided owned = new Provided(
            rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
        );
        final Map<String, String> pairs = Map.of("app", "form");
        final Map<String, Map<String, String>> bound = new Bound(
            Map.of("app", List.of("short-fill", "long-fill")),
            Collections.emptyMap(), Collections.emptyMap(), pairs, owned
        ).all();
        MatcherAssert.assertThat(
            "the more specific (longer) filled prefix must win, not whichever the map yields first",
            new Filled(
                pairs,
                owned,
                new Puts(bound, new Holders(bound, pairs).all()),
                Collections.emptyList()
            ).instead("Φ.node.x.y", "app", "app"),
            Matchers.equalTo("Φ.result")
        );
    }

    @Test
    void namesAFillingOnARingTheWayTheRestOfThePassNamesIt() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put("form", List.of(Map.of("void", "true", "type", "Φ.node.x")));
        final Map<String, String> pairs = new HashMap<>(0);
        pairs.put("app", "form");
        pairs.put("zebra", "alpha");
        pairs.put("alpha", "zebra");
        final Provided owned = new Provided(
            rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
        );
        final Map<String, Map<String, String>> bound = new Bound(
            Map.of("app", List.of("zebra")),
            Collections.emptyMap(), Collections.emptyMap(), pairs, owned
        ).all();
        MatcherAssert.assertThat(
            "a filling that sits on a ring must come back under the name the ring goes by, but it didnt",
            new Filled(
                pairs,
                owned,
                new Puts(bound, new Holders(bound, pairs).all()),
                Collections.emptyList()
            ).instead("Φ.node.x", "app", "app"),
            Matchers.equalTo("alpha")
        );
    }
}
