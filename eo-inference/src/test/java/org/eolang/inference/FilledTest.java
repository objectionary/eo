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
        MatcherAssert.assertThat(
            "an exact fill of the whole answer must win over a fill of one of its prefixes",
            new Filled(
                Map.of("app", List.of("value-x", "value-foo")),
                Map.of("app", "form"),
                new Provided(
                    rows, Collections.emptyMap(),
                    Collections.emptyList(), Collections.emptyMap()
                )
            ).instead("Φ.node.x", "app"),
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
        MatcherAssert.assertThat(
            "the more specific (longer) filled prefix must win, not whichever the map yields first",
            new Filled(
                Map.of("app", List.of("short-fill", "long-fill")),
                Map.of("app", "form"),
                new Provided(
                    rows, Collections.emptyMap(),
                    Collections.emptyList(), Collections.emptyMap()
                )
            ).instead("Φ.node.x.y", "app"),
            Matchers.equalTo("Φ.result")
        );
    }
}
