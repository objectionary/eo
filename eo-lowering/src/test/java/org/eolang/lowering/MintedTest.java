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
}
