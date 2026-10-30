/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Packages}.
 *
 * @since 0.77.0
 */
final class PackagesTest {

    @Test
    void namesThePrefixNobodyFormedAnObjectAt() {
        MatcherAssert.assertThat(
            "the package of the file must be named, but it wasnt",
            new Packages(
                Arrays.asList("Φ.foo.oak", "Φ.foo.oak.leaf", "Φ.app")
            ).all(),
            Matchers.hasItem("Φ.foo")
        );
    }

    @Test
    void leavesOutThePrefixAnObjectAlreadyNames() {
        MatcherAssert.assertThat(
            "a package holding an object of its own is named by that object",
            new Packages(
                Arrays.asList("Φ.string", "Φ.string.printf", "Φ.app")
            ).all(),
            Matchers.not(Matchers.hasItem("Φ.string"))
        );
    }

    @Test
    void namesEveryStepOfAPackageOfSeveralParts() {
        MatcherAssert.assertThat(
            "both steps of com.example must be named, but they werent",
            new Packages(
                Arrays.asList("Φ.com.example.oak", "Φ.app")
            ).all(),
            Matchers.hasItems("Φ.com", "Φ.com.example")
        );
    }

    @Test
    void namesNoPrefixOfAProgramWithoutPackages() {
        MatcherAssert.assertThat(
            "a program whose objects all sit at the root has no package",
            new Packages(
                Arrays.asList("Φ.app", "Φ.app.φ", "Φ.oak")
            ).all(),
            Matchers.not(Matchers.hasItem("Φ.app"))
        );
    }
}
