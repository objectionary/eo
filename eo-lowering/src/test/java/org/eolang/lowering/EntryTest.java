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
 * Test case for {@link Entry}.
 *
 * @since 0.77.0
 */
final class EntryTest {

    @Test
    void namesLocatorWithBoundVoidsAsAtom() {
        MatcherAssert.assertThat(
            "the atom of an entry must be the locator with the bound voids, but it isnt",
            new Entry("s4", "Φ.foo.walk(i,acc)", Arrays.asList("sym:v0", "sym:s2", "sym:v1"), "number")
                .atom(),
            Matchers.equalTo("Φ.foo.walk(i,acc)")
        );
    }

    @Test
    void hasNoBranches() {
        MatcherAssert.assertThat(
            "an entry forks nowhere, but it has branches",
            new Entry("s1", "Φ.foo.g(y)", Arrays.asList("sym:v0", "sym:v1"), "bool").branches(),
            Matchers.empty()
        );
    }
}
