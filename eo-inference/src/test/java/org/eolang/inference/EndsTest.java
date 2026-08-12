/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link Ends}.
 *
 * <p>The pairs arrive one at a time, from the links a rule wrote and from what
 * the loop worked out later, and this is the walking that turns them into one
 * name per type. It knows nothing about EO and everything about chains, so it
 * is asked here directly.</p>
 *
 * @since 0.68.0
 */
final class EndsTest {

    @Test
    void followsChainToItsEnd() {
        final Map<String, String> pairs = new HashMap<>(2);
        pairs.put("Φ.app.φ.α0", "Φ.app.lid");
        pairs.put("Φ.app.lid", "Φ.jar");
        MatcherAssert.assertThat(
            "a copy of a copy must go by the name at the end of the chain, but it didnt",
            new Ends(pairs).names(),
            Matchers.hasEntry("Φ.app.φ.α0", "Φ.jar")
        );
    }

    @Test
    void leavesLonePairAlone() {
        MatcherAssert.assertThat(
            "a pair that leads nowhere further must stay as it is, but it didnt",
            new Ends(Collections.singletonMap("Φ.app.φ", "Φ.jar")).names(),
            Matchers.hasEntry("Φ.app.φ", "Φ.jar")
        );
    }

    @Test
    @Timeout(10L)
    void walksCircleOnlyOnce() {
        final Map<String, String> pairs = new HashMap<>(2);
        pairs.put("Φ.app.one", "Φ.app.two");
        pairs.put("Φ.app.two", "Φ.app.one");
        MatcherAssert.assertThat(
            "a chain that comes back on itself must be walked once, but it wasnt",
            new Ends(pairs).names(),
            Matchers.hasKey("Φ.app.one")
        );
    }
}
