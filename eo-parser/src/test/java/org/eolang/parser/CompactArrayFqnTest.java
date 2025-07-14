/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link CompactArrayFqn}.
 *
 * @since 0.57.2
 */
final class CompactArrayFqnTest {

    @Test
    void buildsFqnForSimpleName() {
        MatcherAssert.assertThat(
            "FQN of the compact array does not match with expected",
            new CompactArrayFqn("foo *1").asString(),
            Matchers.equalTo("foo")
        );
    }
}
