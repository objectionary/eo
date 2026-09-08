/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Minted}.
 * @since 0.76.0
 */
final class MintedTest {

    @Test
    void countsLabelsUp() {
        final Minted minted = new Minted(Collections.singletonMap("x", "number"));
        minted.next();
        MatcherAssert.assertThat(
            "the second label taken must be s2, but it isnt",
            minted.next(),
            Matchers.equalTo("s2")
        );
    }

    @Test
    void namesCarrierOfBoundStep() {
        final Minted minted = new Minted(Collections.singletonMap("x", "number"));
        minted.bind(minted.next(), "bool");
        MatcherAssert.assertThat(
            "a step bound to a forma must carry it, but it doesnt",
            minted.carrier("sym:s1"),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void namesCarrierOfVoidByPosition() {
        final Map<String, String> voids = new LinkedHashMap<>();
        voids.put("x", "number");
        voids.put("b", "bytes");
        MatcherAssert.assertThat(
            "the second void must carry what it declares, but it doesnt",
            new Minted(voids).carrier("sym:v1"),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void refusesStepStillUnbound() {
        final Minted minted = new Minted(Collections.singletonMap("x", "number"));
        minted.next();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> minted.carrier("sym:s1"),
            "a label taken but bound to no forma names no finished step, but it answered"
        );
    }

    @Test
    void handsOverBytesOfViewedNumber() {
        MatcherAssert.assertThat(
            "the bytes of a number must be handed over as bytes, but they arent",
            new Minted(Collections.singletonMap("x", "number"))
                .carried(new Forced(new Symbol("v0", "number"))),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void handsOverFormaOfPlainSymbol() {
        MatcherAssert.assertThat(
            "a plain symbol must hand over the forma of its void, but it doesnt",
            new Minted(Collections.singletonMap("x", "number"))
                .carried(new Symbol("v0", "number")),
            Matchers.equalTo("number")
        );
    }
}
