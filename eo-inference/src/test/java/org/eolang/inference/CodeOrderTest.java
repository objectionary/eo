/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link CodeOrder}.
 *
 * @since 0.76.0
 */
final class CodeOrderTest {

    @Test
    void putsACharacterBeyondTheBasicPlaneAfterOneInsideIt() {
        MatcherAssert.assertThat(
            "a cactus must come after a full-width bang, as its code point does, but it didnt",
            new CodeOrder().compare("a🌵7-2", "a！"),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void putsAPrefixBeforeWhatItStarts() {
        MatcherAssert.assertThat(
            "a locator must come before the locators under it, but it didnt",
            new CodeOrder().compare("Φ.ivy", "Φ.ivy.φ"),
            Matchers.lessThan(0)
        );
    }

    @Test
    void findsTwoEqualLocatorsEqual() {
        MatcherAssert.assertThat(
            "two equal locators must compare equal, but they didnt",
            new CodeOrder().compare("Φ.p🌵can-add", "Φ.p🌵can-add"),
            Matchers.equalTo(0)
        );
    }

    @Test
    void sortsLocatorsTheWayPythonSortsThem() {
        final List<String> sorted = new ArrayList<>(
            Arrays.asList("Φ.oak.p🌵12-4", "Φ.oak.pｚ", "Φ.oak", "Φ.oak.α1")
        );
        sorted.sort(new CodeOrder());
        MatcherAssert.assertThat(
            "the locators must be sorted by code point, but they werent",
            sorted,
            Matchers.contains("Φ.oak", "Φ.oak.pｚ", "Φ.oak.p🌵12-4", "Φ.oak.α1")
        );
    }
}
