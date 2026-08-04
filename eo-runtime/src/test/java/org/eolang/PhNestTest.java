/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhNest}.
 * @since 0.62
 */
final class PhNestTest {

    @Test
    void rejectsDirectPutByPosition() {
        MatcherAssert.assertThat(
            "Direct put by position into a package object must fail fast, but it didn't",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Phi.Φ.take("number").put(0, new Data.ToPhi(42L)),
                "Putting by position straight into a shared package object must be rejected"
            ).getMessage(),
            Matchers.containsString("make a copy first")
        );
    }

    @Test
    void rejectsDirectPutByName() {
        MatcherAssert.assertThat(
            "Direct put by name into a package object must fail fast, but it didn't",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Phi.Φ.take("number").put("x", new Data.ToPhi(42L)),
                "Putting by name straight into a shared package object must be rejected"
            ).getMessage(),
            Matchers.containsString("make a copy first")
        );
    }

    @Test
    void allowsPutAfterCopy() {
        Assertions.assertDoesNotThrow(
            () -> Phi.Φ.take("number").copy().put(0, new Data.ToPhi(42L)),
            "A copy of a package object must accept a put, but it didn't"
        );
    }

    @Test
    void refusesPackageThatWasNeverTranspiled() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhNest("Φ.org.eolang").take("dummy"),
            "A Java package carrying no @XmirPackage must not pass for an EO package, but it did"
        );
    }

    @Test
    void acceptsPackageThatNamesItsXmirOrigin() {
        Assertions.assertDoesNotThrow(
            () -> Phi.Φ.take("string").take("printf"),
            "A Java package naming the EO package it came from must be accepted, but it wasn't"
        );
    }

    @Test
    void handsOutExtensionWithoutBindingPackageAsRho() {
        MatcherAssert.assertThat(
            "Explicit dispatch must leave ρ unbound so the receiver convention stays α0-only, but ρ was set",
            Phi.Φ.take("number").take("power").hasRho(),
            Matchers.is(false)
        );
    }
}
