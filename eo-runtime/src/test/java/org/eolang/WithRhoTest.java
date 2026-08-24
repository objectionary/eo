/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link WithRho}.
 * @since 0.73.4
 */
final class WithRhoTest {

    @Test
    void recognizesTheObjectItWasMadeFrom() {
        final Phi origin = new PhDefault();
        MatcherAssert.assertThat(
            "the object it was made from was not recognized, but it must be",
            new WithRho(origin, origin.copy()).made(origin),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsAnotherObject() {
        final Phi origin = new PhDefault();
        MatcherAssert.assertThat(
            "another object was taken for the one it was made from, but it must not be",
            new WithRho(origin, origin.copy()).made(new PhDefault()),
            Matchers.is(false)
        );
    }
}
