/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Pile}.
 *
 * @since 0.76.0
 */
final class PileTest {

    @Test
    void givesBackLastNumberFirst() {
        final Pile pile = new Pile();
        for (int num = 0; num < 40; ++num) {
            pile.push(num * 3);
        }
        MatcherAssert.assertThat(
            "the number taken off must be the last one put on, but it wasnt",
            pile.pop(),
            Matchers.equalTo(117)
        );
    }

    @Test
    void keepsNumberPutTwice() {
        final Pile pile = new Pile();
        pile.push(9);
        pile.push(9);
        MatcherAssert.assertThat(
            "a number put on twice must be in the pile twice, but it wasnt",
            pile.size(),
            Matchers.equalTo(2)
        );
    }
}
