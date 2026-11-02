/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Program}.
 *
 * @since 0.76.0
 */
final class ProgramTest {

    @Test
    void findsNearestFormationAroundPlace() {
        MatcherAssert.assertThat(
            "the formation right around a place must be the longest one its name starts with, but it wasnt",
            ProgramTest.program().nearest("Φ.kettle.spout.φ.α0.ρ"),
            Matchers.equalTo("Φ.kettle.spout")
        );
    }

    @Test
    void findsNothingAroundRoot() {
        MatcherAssert.assertThat(
            "nothing must be around the root, but something was",
            ProgramTest.program().nearest("Φ"),
            Matchers.emptyString()
        );
    }

    @Test
    void answersPlaceInFormationAroundIt() {
        MatcherAssert.assertThat(
            "a read must be answered in the formation around it, but it wasnt",
            ProgramTest.program().body("Φ.kettle.spout"),
            Matchers.hasItem("Φ.kettle.spout.φ")
        );
    }

    private static Program program() {
        final Formation empty = new Formation(Map.of(), List.of(), Map.of(), Map.of(), List.of());
        return new Program(
            Map.of("Φ", empty, "Φ.kettle", empty, "Φ.kettle.spout", empty),
            Map.of("Φ.kettle.spout.φ", new Read("water", "Φ.kettle.spout.φ.ρ")),
            Map.of("Φ.kettle.spout.φ.ρ", List.of(new Head("ref", "Φ.kettle"))),
            Map.of()
        );
    }
}
