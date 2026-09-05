/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Application}.
 * @since 0.76.0
 */
final class ApplicationTest {

    @Test
    void keepsReceiverFirst() {
        MatcherAssert.assertThat(
            "the keys must start with the receiver, but they dont",
            new Application(
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
            new Application(
                "s1", "L_number_div", Arrays.asList("sym:v0", "number:40-00-")
            ).atom(),
            Matchers.equalTo("L_number_div")
        );
    }

    @Test
    void answersFormaOfAtom() {
        MatcherAssert.assertThat(
            "a comparison must compute a bool, but it doesnt",
            new Application(
                "s1", "L_number_gt", Arrays.asList("sym:v0", "number:40-00-")
            ).forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void nestsNothing() {
        MatcherAssert.assertThat(
            "an application must hold no arms, but it does",
            new Application("s1", "L_bytes_not", Collections.singletonList("sym:v0")).branches(),
            Matchers.empty()
        );
    }
}
