/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Protocol}.
 * @since 0.76.0
 */
final class ProtocolTest {

    @Test
    void answersWithGivenKey() {
        MatcherAssert.assertThat(
            "the answer must come back as given, but it didnt",
            new Protocol(Collections.emptyList(), "sym:v0", "bytes").answer(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void namesCarrier() {
        MatcherAssert.assertThat(
            "the forma must come back as given, but it didnt",
            new Protocol(Collections.emptyList(), "bool:01-", "bool").carrier(),
            Matchers.equalTo("bool")
        );
    }
}
