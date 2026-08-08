/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.ReadSyscall;
import org.eolang.win32.ReadFuncCall;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@code read} system calls.
 * @since 0.40.0
 */
final class ReadSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsNegativeSizeOnPosixRead() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new ReadSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(-7)
            ),
            "A negative posix read size must fail with ExFailure, not NegativeArraySizeException"
        );
    }

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void rejectsNegativeSizeOnWindowsRead() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new ReadFuncCall(Phi.Φ.take("win32").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(-7)
            ),
            "A negative win32 read size must fail with ExFailure, not NegativeArraySizeException"
        );
    }
}
