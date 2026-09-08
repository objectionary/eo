/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link VoidName}.
 * @since 0.74.0
 */
final class VoidNameTest {

    @Test
    void promotesPhiToken() {
        MatcherAssert.assertThat(
            "a `@` parameter must be emitted as `φ` per R-3.4.2 / R-9.3",
            new VoidName("@").asString(),
            Matchers.equalTo("φ")
        );
    }

    @Test
    void promotesRhoToken() {
        MatcherAssert.assertThat(
            "a `^` parameter must be emitted as `ρ` per R-3.4.11 / R-9.3",
            new VoidName("^").asString(),
            Matchers.equalTo("ρ")
        );
    }

    @Test
    void keepsOrdinaryName() {
        MatcherAssert.assertThat(
            "a parameter that is not a scope token must name itself",
            new VoidName("x").asString(),
            Matchers.equalTo("x")
        );
    }
}
