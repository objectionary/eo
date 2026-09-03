/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Step}.
 * @since 0.76.0
 */
final class StepTest {

    @Test
    void keepsReceiverFirst() {
        MatcherAssert.assertThat(
            "the keys must start with the receiver, but they dont",
            new Step(
                "s2",
                "L_bytes_slice",
                Arrays.asList("sym:v0", "number:11-", "sym:s1")
            ).keys().get(0),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void namesAtom() {
        MatcherAssert.assertThat(
            "the λ name must come back as given, but it didnt",
            new Step(
                "s1", "L_number_div", Arrays.asList("sym:v0", "number:40-00-")
            ).atom(),
            Matchers.equalTo("L_number_div")
        );
    }
}
