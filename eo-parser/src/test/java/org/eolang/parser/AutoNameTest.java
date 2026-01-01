/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link AutoName}.
 *
 * @since 0.58.1
 */
final class AutoNameTest {

    @Test
    void generatesAutoNameForLineAndPos() {
        MatcherAssert.assertThat(
            "Auto name does not match with expected",
            new AutoName(42, 13).asString(),
            Matchers.equalTo("a\uD83C\uDF354213")
        );
    }
}
