/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Carrier}.
 *
 * @since 0.77.0
 */
final class CarrierTest {

    @Test
    void knowsDataObjects() {
        MatcherAssert.assertThat(
            "the tuple must be known as a data object, but it isnt",
            new Carrier("Φ.tuple").data(),
            Matchers.is(true)
        );
    }

    @Test
    void namesFormaOfBoolState() {
        MatcherAssert.assertThat(
            "a state of the bool must carry the bool forma, but it doesnt",
            new Carrier("Φ.true").forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void staysSilentOnForeignLocator() {
        MatcherAssert.assertThat(
            "a locator of no data object cannot carry a forma, but it does",
            new Carrier("Φ.foo.bar").forma(),
            Matchers.is(Matchers.emptyString())
        );
    }
}
