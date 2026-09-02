/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Carried}.
 * @since 0.70.0
 */
final class CarriedTest {

    @Test
    void walksTwoHopsInARow() {
        final Map<String, Map<String, Type>> handed = new LinkedHashMap<>(0);
        handed.put(
            "Φ.near.x", Collections.singletonMap("Φ.far.y", new Var("Φ.far.y"))
        );
        handed.put(
            "Φ.far.y", Collections.singletonMap("Φ.end.z", new Var("Φ.end.z"))
        );
        MatcherAssert.assertThat(
            "what fills the far void must reach the near one, but it stopped halfway",
            new Carried(
                Collections.singletonMap(
                    "Φ.end.z", Collections.singletonMap("Φ.oak", new Ref("Φ.oak"))
                ),
                handed
            ).all().get("Φ.near.x"),
            Matchers.hasKey("Φ.oak")
        );
    }

    @Test
    void keepsTheVoidWhereNothingIsCarried() {
        MatcherAssert.assertThat(
            "a void whose only source is empty must name that source, but it named nothing",
            new Carried(
                Collections.emptyMap(),
                Collections.singletonMap(
                    "Φ.near.x", Collections.singletonMap("Φ.far.y", new Var("Φ.far.y"))
                )
            ).all().get("Φ.near.x"),
            Matchers.hasKey("Φ.far.y")
        );
    }

    @Test
    void doesNotLoopOnAVoidHandedToItself() {
        MatcherAssert.assertThat(
            "a void that fills itself must be answered, but the walk never came back",
            new Carried(
                Collections.singletonMap(
                    "Φ.self.x", Collections.singletonMap("Φ.oak", new Ref("Φ.oak"))
                ),
                Collections.singletonMap(
                    "Φ.self.x", Collections.singletonMap("Φ.self.x", new Var("Φ.self.x"))
                )
            ).all().get("Φ.self.x"),
            Matchers.hasKey("Φ.oak")
        );
    }
}
