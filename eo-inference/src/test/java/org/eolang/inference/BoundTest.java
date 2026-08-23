/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
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
    void fillsTheSecondVoidWhenAChainAppliesTwice() {
        final Map<String, Collection<Map<String, String>>> rows = Map.of(
            "pair",
            List.of(
                Map.of("void", "true", "type", "pair.x"),
                Map.of("void", "true", "type", "pair.y")
            )
        );
        final Map<String, Map<String, String>> all = new Bound(
            Map.of("half", List.of("half.α0"), "full", List.of("full.α0")),
            Map.of("half", "pair", "full", "half"),
            new Provided(rows, Collections.emptyMap(), Collections.emptyList(), Collections.emptyMap())
        ).all();
        MatcherAssert.assertThat(
            "half must still fill the first void, since nothing filled it before",
            all.get("half"),
            Matchers.equalTo(Map.of("pair.x", "half.α0"))
        );
        MatcherAssert.assertThat(
            "full copies half, so its argument must land on the void half left free",
            all.get("full"),
            Matchers.equalTo(Map.of("pair.y", "full.α0"))
        );
    }
}
