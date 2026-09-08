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
    void fillsAFreshVoidForEachApplicationOfAChain() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.app.pair",
            List.of(
                Map.of("void", "true", "type", "Φ.app.pair.x"),
                Map.of("void", "true", "type", "Φ.app.pair.y")
            )
        );
        final Map<String, List<String>> args = new HashMap<>(0);
        args.put("Φ.app.half", List.of("Φ.app.one"));
        args.put("Φ.app.full", List.of("Φ.app.two"));
        final Map<String, String> pairs = new HashMap<>(0);
        pairs.put("Φ.app.half", "Φ.app.pair");
        pairs.put("Φ.app.full", "Φ.app.half");
        MatcherAssert.assertThat(
            "the second application of a chain must fill the void the first one left empty",
            new Bound(
                args, Collections.emptyMap(), Collections.emptyMap(), pairs,
                new Provided(
                    rows, Collections.emptyMap(),
                    Collections.emptyList(), Collections.emptyMap()
                )
            ).all(),
            Matchers.equalTo(
                Map.of(
                    "Φ.app.half", Map.of("Φ.app.pair.x", "Φ.app.one"),
                    "Φ.app.full", Map.of("Φ.app.pair.y", "Φ.app.two")
                )
            )
        );
    }

    @Test
    void namesBothVoidsOfAnApplicationBoundEntirelyByName() {
        MatcherAssert.assertThat(
            "both voids named inline must show up, keyed by the void they were bound to",
            new Bound(
                Map.of("only", List.of()),
                Map.of("only", Map.of("y", "only.y", "x", "only.x")),
                Collections.emptyMap(),
                Map.of("only", "pair"),
                new Provided(
                    Map.of(
                        "pair",
                        List.of(
                            Map.of("void", "true", "name", "x", "type", "pair.x"),
                            Map.of("void", "true", "name", "y", "type", "pair.y")
                        )
                    ),
                    Collections.emptyMap(),
                    Collections.emptyList(),
                    Collections.emptyMap()
                )
            ).all().get("only"),
            Matchers.equalTo(Map.of("pair.y", "only.y", "pair.x", "only.x"))
        );
    }

    @Test
    void passesArgumentsOnToAFormationHeldByAVoidMidwayAlongAChain() {
        final Map<String, Collection<Map<String, String>>> rows = new HashMap<>(0);
        rows.put(
            "Φ.app.gate",
            List.of(Map.of("void", "true", "name", "func", "type", "Φ.app.gate.func"))
        );
        rows.put(
            "Φ.app.leaf",
            List.of(Map.of("void", "true", "name", "item", "type", "Φ.app.leaf.item"))
        );
        rows.put(
            "Φ.app.twig",
            List.of(Map.of("void", "true", "name", "item", "type", "Φ.app.twig.item"))
        );
        final Map<String, List<String>> args = new HashMap<>(0);
        args.put("Φ.app.call", List.of("Φ.app.arg"));
        args.put("Φ.app.put", List.of("Φ.app.leaf"));
        final Map<String, String> pairs = new HashMap<>(0);
        pairs.put("Φ.app.call", "Φ.app.gate.func");
        pairs.put("Φ.app.gate.func", "Φ.app.twig");
        pairs.put("Φ.app.put", "Φ.app.gate");
        MatcherAssert.assertThat(
            "an argument must reach the formation a void in the middle of the chain holds, but it didnt",
            new Bound(
                args, Collections.emptyMap(), Collections.emptyMap(), pairs,
                new Provided(
                    rows, Collections.emptyMap(),
                    Collections.emptyList(), Collections.emptyMap()
                )
            ).all().get("Φ.app.call"),
            Matchers.equalTo(
                Map.of("Φ.app.twig.item", "Φ.app.arg", "Φ.app.leaf.item", "Φ.app.arg")
            )
        );
    }

    @Test
    void fillsAVoidFromAnApplicationInsideARing() {
        final Map<String, String> pairs = new HashMap<>(0);
        pairs.put("Φ.app.zebra", "Φ.app.alpha");
        pairs.put("Φ.app.alpha", "Φ.app.zebra");
        MatcherAssert.assertThat(
            "an application on a ring must still fill the void it names, but it didnt",
            new Bound(
                Map.of("Φ.app.zebra", List.of("Φ.app.one")),
                Collections.emptyMap(),
                Collections.emptyMap(),
                pairs,
                new Provided(
                    Map.of(
                        "Φ.app.alpha",
                        List.of(Map.of("void", "true", "type", "Φ.app.alpha.x"))
                    ),
                    Collections.emptyMap(),
                    Collections.emptyList(),
                    Collections.emptyMap()
                )
            ).all().get("Φ.app.zebra"),
            Matchers.equalTo(Map.of("Φ.app.alpha.x", "Φ.app.one"))
        );
    }

    @Test
    void readsTheReceiverOfARingOffTheNameTheRingGoesBy() {
        final Map<String, String> pairs = new HashMap<>(0);
        pairs.put("Φ.app.zebra", "Φ.app.alpha");
        pairs.put("Φ.app.alpha", "Φ.app.zebra");
        MatcherAssert.assertThat(
            "a dispatch into a ring must read its ρ off the name the ring goes by, but it didnt",
            new Bound(
                Collections.emptyMap(),
                Collections.emptyMap(),
                Map.of("Φ.app.zebra", "Φ.app.thing"),
                pairs,
                new Provided(
                    Map.of(
                        "Φ.app.alpha",
                        List.of(Map.of("void", "true", "name", "ρ", "type", "Φ.app.alpha.ρ"))
                    ),
                    Collections.emptyMap(),
                    Collections.emptyList(),
                    Collections.emptyMap()
                )
            ).all().get("Φ.app.zebra"),
            Matchers.equalTo(Map.of("Φ.app.alpha.ρ", "Φ.app.thing"))
        );
    }
}
