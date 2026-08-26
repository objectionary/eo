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
 * Test case for {@link Bound}.
 * @since 0.69.0
 */
final class BoundTest {

    @Test
    void fillsTheFirstVoidForALoneApplication() {
        MatcherAssert.assertThat(
            "a lone application must fill the first void of what it copies",
            new Bound(
                Map.of("Φ.app.half", List.of("Φ.app.one")),
                Map.of("Φ.app.half", "Φ.app.pair"),
                BoundTest.pair()
            ).all(),
            Matchers.equalTo(
                Map.of("Φ.app.half", Map.of("Φ.app.pair.x", "Φ.app.one"))
            )
        );
    }

    @Test
    void fillsAFreshVoidForEachApplicationInAChain() {
        final Map<String, List<String>> args = new HashMap<>(0);
        args.put("Φ.app.half", List.of("Φ.app.one"));
        args.put("Φ.app.full", List.of("Φ.app.two"));
        final Map<String, String> pairs = new HashMap<>(0);
        pairs.put("Φ.app.half", "Φ.app.pair");
        pairs.put("Φ.app.full", "Φ.app.half");
        MatcherAssert.assertThat(
            "the second application in a copy chain must fill the void the first one left empty",
            new Bound(args, pairs, BoundTest.pair()).all(),
            Matchers.allOf(
                Matchers.hasEntry(
                    "Φ.app.half", Map.of("Φ.app.pair.x", "Φ.app.one")
                ),
                Matchers.hasEntry(
                    "Φ.app.full", Map.of("Φ.app.pair.y", "Φ.app.two")
                )
            )
        );
    }

    private static Provided pair() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.app.pair",
            List.of(
                Map.of("void", "true", "type", "Φ.app.pair.x"),
                Map.of("void", "true", "type", "Φ.app.pair.y")
            )
        );
        return new Provided(
            rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap()
        );
    }
}
