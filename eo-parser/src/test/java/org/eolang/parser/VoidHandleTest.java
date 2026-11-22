/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link VoidHandle}.
 *
 * @since 0.64.0
 */
final class VoidHandleTest {

    @Test
    void readsHandleOfRhoToken() {
        MatcherAssert.assertThat(
            "a `^name` parameter must carry `name` as its file-local handle",
            new VoidHandle("^k9-q").asString(),
            Matchers.equalTo("k9-q")
        );
    }

    @Test
    void carriesNoHandleForBareRho() {
        MatcherAssert.assertThat(
            "a bare `^` parameter must not carry a handle",
            new VoidHandle("^").asString(),
            Matchers.emptyString()
        );
    }

    @Test
    void carriesNoHandleForOrdinaryName() {
        MatcherAssert.assertThat(
            "an ordinary parameter must not carry a handle",
            new VoidHandle("zq3").asString(),
            Matchers.emptyString()
        );
    }
}
