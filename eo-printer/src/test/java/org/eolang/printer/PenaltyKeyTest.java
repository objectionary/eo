/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PenaltyKey} weights and their overrides.
 * @since 0.57.0
 */
final class PenaltyKeyTest {

    @Test
    void chargesDefaultsForAbsentKeys() {
        MatcherAssert.assertThat(
            "An empty weights map should behave exactly like the defaults",
            new Penalty("42.gt (bar.hello 88) > [] > foo", Collections.emptyMap()).points(),
            Matchers.equalTo(new Penalty("42.gt (bar.hello 88) > [] > foo").points())
        );
    }

    @Test
    void honoursOverriddenBracketWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.BRACKET, 100);
        weights.put(PenaltyKey.SYMBOL, 0);
        weights.put(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            "A single parenthesis should cost the overridden weight",
            new Penalty("42.gt (bar.hello 88) > [] > foo", weights).points(),
            Matchers.equalTo(100)
        );
    }

    @Test
    void honoursOverriddenStepWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.STEP, 4);
        weights.put(PenaltyKey.SYMBOL, 0);
        weights.put(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            "Two levels of four-space indentation should cost four points",
            new Penalty(
                String.join(System.lineSeparator(), "[] > foo", "        bar"),
                weights
            ).points(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void honoursOverriddenWidthWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.WIDTH, 40);
        weights.put(PenaltyKey.SYMBOL, 0);
        MatcherAssert.assertThat(
            "Characters past the overridden 40th column should be charged",
            new Penalty(
                String.join("", java.util.Collections.nCopies(45, "x")), weights
            ).points(),
            Matchers.equalTo(15)
        );
    }

    @Test
    void honoursOverriddenApplicationWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.SPACE, 100);
        weights.put(PenaltyKey.SYMBOL, 0);
        MatcherAssert.assertThat(
            "A single application space should cost the overridden weight",
            new Penalty("foo bar", weights).points(),
            Matchers.equalTo(100)
        );
    }
}
