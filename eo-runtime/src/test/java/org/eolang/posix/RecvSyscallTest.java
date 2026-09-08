/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

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
 * Test case for {@link RecvSyscall}.
 * @since 0.57.0
 */
final class RecvSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void survivesFailedRecvWithoutCrashing() {
        MatcherAssert.assertThat(
            "A failed posix recv must report code -1 with empty output, not crash",
            RecvSyscallTest.outcome(
                new RecvSyscall(Phi.Φ.take("posix").copy()).make(
                    new Data.ToPhi(-1), new Data.ToPhi(16), new Data.ToPhi(0)
                )
            ),
            Matchers.contains(-1, 0)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsNegativeSizeOnPosixRecv() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(-7), new Data.ToPhi(0)
            ),
            "A negative posix recv size must fail with ExFailure, not NegativeArraySizeException"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsFractionalSizeOnPosixRecv() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(1.5), new Data.ToPhi(0)
            ),
            "A fractional posix recv size must fail with ExFailure, not receive a truncated count"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsInfiniteSizeOnPosixRecv() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(Double.POSITIVE_INFINITY), new Data.ToPhi(0)
            ),
            "An infinite posix recv size must fail with ExFailure, not allocate the largest int"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsSizeLargerThanTheHeap() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new RecvSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(Integer.MAX_VALUE), new Data.ToPhi(0)
            ),
            "A posix recv of more bytes than the heap holds must fail with ExFailure"
        );
    }

    private static List<Integer> outcome(final Phi result) {
        return Arrays.asList(
            new Dataized(result.take("code")).asNumber().intValue(),
            ((byte[]) new Dataized(result.take("output")).take()).length
        );
    }
}
