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
 * Test case for {@link Formation}.
 *
 * @since 0.76.0
 */
final class FormationTest {

    @Test
    void givesEachVoidBitOfItsPlaceInOrder() {
        MatcherAssert.assertThat(
            "the third void must get the third bit, but it didnt",
            new Formation(
                Map.of(), List.of("width", "height", "depth"), Map.of(), Map.of(), List.of()
            ).bit("depth"),
            Matchers.equalTo(4L)
        );
    }

    @Test
    void givesRhoBitOfItsOwn() {
        MatcherAssert.assertThat(
            "ρ must get its own bit, but it didnt",
            new Formation(Map.of(), List.of("ρ"), Map.of(), Map.of(), List.of()).bit("ρ"),
            Matchers.equalTo(Formation.RHO)
        );
    }

    @Test
    void givesNoBitToNameThatIsNotVoid() {
        MatcherAssert.assertThat(
            "a name that is not a void must get no bit, but it did",
            new Formation(Map.of(), List.of("key"), Map.of(), Map.of(), List.of()).bit("value"),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void saysNothingOfHoldsOfAttributeItLacks() {
        MatcherAssert.assertThat(
            "an attribute that is not there must hold nothing, but it held something",
            new Formation(
                Map.of("size", new Attribute(true, "Φ.number")), List.of("size"),
                Map.of(), Map.of(), List.of()
            ).holds("weight"),
            Matchers.emptyString()
        );
    }
}
