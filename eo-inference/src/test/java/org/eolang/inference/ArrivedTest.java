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
 * Test case for {@link Arrived}.
 *
 * @since 0.71.0
 */
final class ArrivedTest {

    @Test
    void walksARunOfNamesOneAtATime() {
        MatcherAssert.assertThat(
            "the second name must be asked of what the first arrived at, not of where it started",
            new Arrived(
                new Provided(
                    Map.of(
                        "Φ.dial", List.of(Map.of("name", "listen", "type", "Φ.dial.listen")),
                        "Φ.dial.listen", List.of(Map.of("name", "size", "type", "Φ.hertz"))
                    ),
                    Collections.emptyMap(),
                    Collections.emptyList(),
                    Collections.emptyMap()
                )
            ).names("Φ.dial", "listen.size"),
            Matchers.equalTo("Φ.hertz")
        );
    }

    @Test
    void dropsThemAllWhenOneArrivesNowhere() {
        MatcherAssert.assertThat(
            "a name that one of the objects cannot answer must leave nothing of the choice",
            new Arrived(
                new Provided(
                    Map.of(
                        "Φ.dial", List.of(Map.of("name", "listen", "type", "Φ.dial.listen")),
                        "Φ.clock", List.of(Map.of("name", "tick", "type", "Φ.clock.tick"))
                    ),
                    Collections.emptyMap(),
                    Collections.emptyList(),
                    Collections.emptyMap()
                )
            ).names(List.of("Φ.dial", "Φ.clock"), "listen"),
            Matchers.empty()
        );
    }
}
