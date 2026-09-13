/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
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
            new Box(Arrays.asList("L_box_3", "Φ.foo.f", "number", "-", "a:number b:bool")).voids(),
            Matchers.hasEntry("b", "bool")
        );
    }

    @Test
    void knowsWhenBodyNeverTouchesParent() {
        MatcherAssert.assertThat(
            "a dash in the parent column means the body never reaches ρ, but it reached",
            new Box(Arrays.asList("L_box_3", "Φ.foo.f", "number", "-", "")).reaches(),
            Matchers.is(false)
        );
    }

    @Test
    void spellsLineBackWithTabs() {
        MatcherAssert.assertThat(
            "the line must come back tab separated, but it didnt",
            new Box(Arrays.asList("L_box_1", "Φ.foo.g", "bool", "number", "x:number")).line(),
            Matchers.equalTo("L_box_1\tΦ.foo.g\tbool\tnumber\tx:number")
        );
    }
}
