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
 * Test case for {@link Dispatch}.
 * @since 0.76.0
 */
final class DispatchTest {

    @Test
    void namesMethodWithDot() {
        MatcherAssert.assertThat(
            "the step must name its method dot-prefixed, the way EO dispatches, but it doesnt",
            new Dispatch("s1", "minus", Arrays.asList("sym:v0", "number:11-"), "number").atom(),
            Matchers.equalTo(".minus")
        );
    }

    @Test
    void carriesGivenForma() {
        MatcherAssert.assertThat(
            "the step must carry the forma it was minted with, but it doesnt",
            new Dispatch("s1", "length", Collections.singletonList("sym:v0"), "object").forma(),
            Matchers.equalTo("object")
        );
    }

    @Test
    void readsReceiverFirst() {
        MatcherAssert.assertThat(
            "the keys must come back with the receiver first, but they dont",
            new Dispatch("s1", "minus", Arrays.asList("sym:v0", "number:11-"), "number").keys(),
            Matchers.contains("sym:v0", "number:11-")
        );
    }

    @Test
    void nestsNothing() {
        MatcherAssert.assertThat(
            "a call nests no protocol, but it does",
            new Dispatch("s1", "neg", Collections.singletonList("sym:v0"), "number").branches(),
            Matchers.empty()
        );
    }
}
