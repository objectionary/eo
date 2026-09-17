/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link RecvFuncCall}.
 *
 * @since 0.57.0
 */
final class RecvFuncCallTest {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void survivesFailedRecvWithoutCrashing() {
        MatcherAssert.assertThat(
            "A failed win32 recv must report code -1 with empty output, not crash",
            RecvFuncCallTest.outcome(
                new RecvFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(-1), new Data.ToPhi(16), new Data.ToPhi(0)
                )
            ),
            Matchers.contains(-1, 0)
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void rejectsNegativeSizeOnWindowsRecv() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvFuncCall(Phi.Φ.take("win32").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(-7), new Data.ToPhi(0)
            ),
            "A negative win32 recv size must fail with ExFailure, not NegativeArraySizeException"
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void rejectsFractionalSizeOnWindowsRecv() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvFuncCall(Phi.Φ.take("win32").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(1.5), new Data.ToPhi(0)
            ),
            "A fractional win32 recv size must fail with ExFailure, not receive a truncated count"
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void rejectsInfiniteSizeOnWindowsRecv() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvFuncCall(Phi.Φ.take("win32").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(Double.POSITIVE_INFINITY), new Data.ToPhi(0)
            ),
            "An infinite win32 recv size must fail with ExFailure, not allocate the largest int"
        );
    }

    private static List<Integer> outcome(final Phi result) {
        return Arrays.asList(
            new Dataized(result.take("code")).asNumber().intValue(),
            ((byte[]) new Dataized(result.take("output")).take()).length
        );
    }
}
