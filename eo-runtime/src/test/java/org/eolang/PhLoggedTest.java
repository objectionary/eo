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
 * @since 0.29.0
 */
final class PhLoggedTest {

    @Test
    void delegatesTermToOrigin() {
        MatcherAssert.assertThat(
            "PhLogged must delegate φ-term to its origin, but it didnt",
            new PhLogged(new PhDefault(new byte[] {(byte) 0x01})).φTerm(),
            Matchers.equalTo("[D> 01-]")
        );
    }

    @Test
    void copiesOrigin() {
        MatcherAssert.assertThat(
            "Copy of PhLogged should return the original Phi, but it didn't",
            new PhLogged(Phi.Φ).copy(),
            Matchers.equalTo(Phi.Φ)
        );
    }

    @Test
    void doesNotEqualOrigin() {
        MatcherAssert.assertThat(
            "a logged object must not compare equal to the Phi it wraps",
            new PhLogged(Phi.Φ),
            Matchers.not(Matchers.equalTo(Phi.Φ))
        );
    }

    @Test
    void equalsToItself() {
        final Phi logged = new PhLogged(new PhDefault());
        MatcherAssert.assertThat(
            "PhLogged asks its origin about equality, and must still answer for itself first",
            logged,
            Matchers.equalTo(logged)
        );
    }

    @Test
    void leavesOriginUnequalToIt() {
        final Phi phi = new PhDefault();
        MatcherAssert.assertThat(
            "Borrowing the origin's hash code cannot make the origin equal to PhLogged, which is another object",
            phi,
            Matchers.not(Matchers.equalTo(new PhLogged(phi)))
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

    @Test
    void keepsLoggingWrapperAfterCopy() {
        MatcherAssert.assertThat(
            "copy() must remain wrapped in PhLogged, so tracing continues, but it didn't",
            new PhLogged(Phi.Φ).copy(),
            Matchers.instanceOf(PhLogged.class)
        );
    }

    @Test
    void keepsLoggingWrapperAfterNormalized() {
        MatcherAssert.assertThat(
            "normalized() must remain wrapped in PhLogged, so tracing continues, but it didn't",
            new PhLogged(Phi.Φ).normalized(),
            Matchers.instanceOf(PhLogged.class)
        );
    }
}
