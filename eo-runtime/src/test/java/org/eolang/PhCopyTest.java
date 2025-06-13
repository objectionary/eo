/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhCopy}.
 *
 * @since 0.16
 */
final class PhCopyTest {

    @Test
    void makesObjectCopy() {
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhCopy(new Data.ToPhi(1))
            ).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    void hasTheSameFormaAsCopied() {
        final Phi phi = new Data.ToPhi(1);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            phi.forma(),
            Matchers.equalTo(
                phi.copy().forma()
            )
        );
    }
}
