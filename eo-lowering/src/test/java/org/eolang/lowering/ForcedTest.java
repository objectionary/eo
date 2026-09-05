/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Forced}.
 * @since 0.76.0
 */
final class ForcedTest {

    @Test
    void rendersViewOfNumberAsDispatch() {
        MatcherAssert.assertThat(
            "the bytes of a number must be asked of its carrier, but they arent",
            new Forced(new Symbol("v0", "number")).phi(),
            Matchers.equalTo("Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧)).as-bytes")
        );
    }

    @Test
    void rendersViewOfBytesAsThemselves() {
        MatcherAssert.assertThat(
            "the bytes of bytes must be the bytes, but they arent",
            new Forced(new Symbol("s1", "bytes")).phi(),
            Matchers.equalTo("Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_s1 ⟧)")
        );
    }

    @Test
    void keepsKeyOfSymbol() {
        MatcherAssert.assertThat(
            "a symbol seen as bytes must keep its key, but it doesnt",
            new Forced(new Symbol("s1", "number")).key(),
            Matchers.equalTo("sym:s1")
        );
    }

    @Test
    void turnsLiteralIntoBytes() {
        MatcherAssert.assertThat(
            "a literal seen as bytes must carry bytes, but it doesnt",
            new Forced(new Literal("number", "40-00-00-00-00-00-00-00")).key(),
            Matchers.equalTo("bytes:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void carriesBytesOnceSettled() {
        MatcherAssert.assertThat(
            "the view of a known value must carry bytes, but it doesnt",
            new Forced(new Symbol("v0", "number")).forma(),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void carriesNothingBeforeSettling() {
        MatcherAssert.assertThat(
            "the view of a site must carry nothing yet, but it does",
            new Forced(ForcedTest.square()).forma(),
            Matchers.equalTo("")
        );
    }

    @Test
    void swapsInsideTheView() {
        MatcherAssert.assertThat(
            "the view must follow the site it wraps into its step, but it doesnt",
            new Forced(ForcedTest.square())
                .swapped(ForcedTest.shape(), new Symbol("s1", "number"))
                .key(),
            Matchers.equalTo("sym:s1")
        );
    }

    private static Shape shape() {
        return new Shape(
            "times", "sym:v0",
            Collections.singletonList(new Binding("α0", new Symbol("v0", "number")))
        );
    }

    private static Term square() {
        return new Site(
            "times",
            new Symbol("v0", "number"),
            Collections.singletonList(new Binding("α0", new Symbol("v0", "number")))
        );
    }
}
