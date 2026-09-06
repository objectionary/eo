/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Rooted}.
 * @since 0.72.0
 */
final class RootedTest {

    @Test
    void namesTheNearestVoidTheNameIsTakenOff() {
        MatcherAssert.assertThat(
            "the void closest to the name must be the one it is rooted at, but a further one came back",
            new Rooted(Arrays.asList("Φ.pump.x", "Φ.pump.x.hose")).names("Φ.pump.x.hose.tip"),
            Matchers.equalTo("Φ.pump.x.hose")
        );
    }

    @Test
    void namesNothingForANameRootedAtNoVoidOfThese() {
        MatcherAssert.assertThat(
            "a name that goes back to none of these voids must come back empty, but it named one",
            new Rooted(Collections.singletonList("Φ.pump.x")).names("Φ.pump.y.hose"),
            Matchers.equalTo("")
        );
    }
}
