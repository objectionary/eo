/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link WriteSyscall}.
 *
 * @since 0.57.0
 */
final class WriteSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsNegativeFractionalSizeOnPosixWrite() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new WriteSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(new byte[]{1, 2}), new Data.ToPhi(-0.5)
            ),
            "A negative fractional posix write size must fail with ExFailure, not report a false zero write"
        );
    }
}
