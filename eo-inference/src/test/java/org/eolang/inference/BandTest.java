/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Band}.
 * @since 0.71.0
 */
final class BandTest {

    @Test
    void namesTheBandOfAVoidAnAtomFills() {
        MatcherAssert.assertThat(
            "a void an atom fills must have a band of its own, but it took another",
            new Band(
                new Answer("Φ.reply.code.size", 1, Collections.emptyList(), true)
            ).name(),
            Matchers.equalTo("atom")
        );
    }

    @Test
    void ranksAVoidTheCallersFillWorseThanOneAnAtomFills() {
        MatcherAssert.assertThat(
            "a void whose callers disagree is less known than one an atom fills, but it ranked above",
            new Band(new Answer("Φ.bool.if", 1)).rank(),
            Matchers.lessThan(
                new Band(
                    new Answer("Φ.reply.code", 1, Collections.emptyList(), true)
                ).rank()
            )
        );
    }
}
