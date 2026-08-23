/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

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
        MatcherAssert.assertThat(
            "full copies half, so its argument must land on the void half left free",
            new Bound(
                Map.of("half", List.of("half.α0"), "full", List.of("full.α0")),
                Map.of("half", "pair", "full", "half"),
                new Provided(
                    Map.of(
                        "pair",
                        List.of(
                            Map.of("void", "true", "type", "pair.x"),
                            Map.of("void", "true", "type", "pair.y")
                        )
                    ),
                    Collections.emptyMap(),
                    Collections.emptyList(),
                    Collections.emptyMap()
                )
            ).all(),
            Matchers.equalTo(
                Map.of(
                    "half", Map.of("pair.x", "half.α0"),
                    "full", Map.of("pair.y", "full.α0")
                )
            )
        );
    }
}
