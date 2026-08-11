/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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

    /**
     * The recv outcome as a {@code [code, output-length]} pair.
     * @param result The dataizable recv result
     * @return The exit code followed by the output byte count
     */
    private static List<Integer> outcome(final Phi result) {
        return Arrays.asList(
            new Dataized(result.take("code")).asNumber().intValue(),
            ((byte[]) new Dataized(result.take("output")).take()).length
        );
    }
}
