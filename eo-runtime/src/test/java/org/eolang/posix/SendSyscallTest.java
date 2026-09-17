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
 * Test case for {@link SendSyscall}.
 *
 * @since 0.57.0
 */
final class SendSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsNegativeFractionalSizeOnPosixSend() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new SendSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(new byte[]{1, 2}),
                new Data.ToPhi(-0.5), new Data.ToPhi(0)
            ),
            "A negative fractional posix send size must fail with ExFailure, not report a false zero send"
        );
    }
}
