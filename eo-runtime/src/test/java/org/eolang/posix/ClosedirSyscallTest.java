/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ClosedirSyscall}.
 * @since 0.76.0
 */
final class ClosedirSyscallTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "closing a stream twice must be refused, since the second close would free a freed pointer",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new ClosedirSyscall(Phi.Φ.take("posix").copy()).make(
                    new Data.ToPhi(-1)
                ),
                "closing a stream that was never opened was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("'dirp' argument of closedir")
        );
    }
}
