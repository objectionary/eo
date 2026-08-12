/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Agreed}.
 *
 * <p>The loop asks what a computed object turns out to be once for every copy
 * of the formation it stands in, and this is what decides whether the answers
 * add up to a fact. It knows nothing about EO and everything about counting,
 * so it is asked here directly.</p>
 *
 * @since 0.68.0
 */
final class AgreedTest {

    @Test
    void believesAnswerNothingContradicts() {
        MatcherAssert.assertThat(
            "an object asked about once must go by the answer that came back, but it didnt",
            new Agreed(
                Collections.singletonMap(
                    "Φ.use.φ", Collections.singletonList("Φ.t.next")
                )
            ).names(),
            Matchers.hasEntry("Φ.use.φ", "Φ.t.next")
        );
    }

    @Test
    void leavesOutLocatorThatCameOutTwoWays() {
        MatcherAssert.assertThat(
            "a locator two copies answer differently about must be left out, but it wasnt",
            new Agreed(
                Collections.singletonMap(
                    "Φ.use.φ", Arrays.asList("Φ.t.next", "Φ.u.next")
                )
            ).names(),
            Matchers.not(Matchers.hasKey("Φ.use.φ"))
        );
    }

    @Test
    void keepsTheOthersWhenOneIsMuddled() {
        final Map<String, Collection<String>> answers = new HashMap<>(2);
        answers.put("Φ.use.φ", Arrays.asList("Φ.t.next", "Φ.u.next"));
        answers.put("Φ.ask.φ", Collections.singletonList("Φ.t.lid"));
        MatcherAssert.assertThat(
            "a locator that came out one way must survive its muddled neighbour, but it didnt",
            new Agreed(answers).names(),
            Matchers.hasEntry("Φ.ask.φ", "Φ.t.lid")
        );
    }
}
