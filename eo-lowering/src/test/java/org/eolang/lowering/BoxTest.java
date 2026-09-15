/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Box}.
 *
 * @since 0.77.0
 */
final class BoxTest {

    @Test
    void readsVoidsWithFormas() {
        MatcherAssert.assertThat(
            "the voids must be read in order with their formas, but they werent",
            new Box(
                Map.of(
                    "locator", "Φ.foo.f", "carrier", "number", "parent", "-",
                    "voids", "a:number b:bool"
                )
            ).voids(),
            Matchers.hasEntry("b", "bool")
        );
    }

    @Test
    void knowsWhenBodyNeverTouchesParent() {
        MatcherAssert.assertThat(
            "a dash in the parent column means the body never reaches ρ, but it reached",
            new Box(
                Map.of(
                    "locator", "Φ.foo.f", "carrier", "number", "parent", "-",
                    "voids", ""
                )
            ).reaches(),
            Matchers.is(false)
        );
    }

    @Test
    void namesItselfAfterTheLastSegmentOfTheLocator() {
        MatcherAssert.assertThat(
            "the box must know the name its parent holds it by, but it doesnt",
            new Box(
                Map.of(
                    "locator", "Φ.foo.bar.f", "carrier", "number", "parent", "-",
                    "voids", ""
                )
            ).name(),
            Matchers.equalTo("f")
        );
    }
}
