/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhLogged}.
 *
 * @since 0.29.0
 */
final class PhLoggedTest {

    @Test
    void copiesOrigin() {
        MatcherAssert.assertThat(
            "Copy of PhLogged should return the original Phi, but it didn't",
            new PhLogged(Phi.Φ).copy(),
            Matchers.equalTo(Phi.Φ)
        );
    }

    @Test
    void returnsOriginHashCode() {
        MatcherAssert.assertThat(
            "HashCode of PhLogged should return the original hashCode, but it didn't",
            new PhLogged(Phi.Φ).hashCode(),
            Matchers.equalTo(Phi.Φ.hashCode())
        );
    }

    @Test
    void equalsToOrigin() {
        MatcherAssert.assertThat(
            "PhLogged should be equlas to the original Phi, but it didn't",
            new PhLogged(Phi.Φ),
            Matchers.equalTo(Phi.Φ)
        );
    }

    @Test
    void getsOriginLocator() {
        final Phi phi = Phi.Φ;
        MatcherAssert.assertThat(
            "Locator of PhLogged should be equlas to the original, but it didn't",
            new PhLogged(phi).locator(),
            Matchers.equalTo(phi.locator())
        );
    }
}
