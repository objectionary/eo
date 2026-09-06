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
 * Test case for {@link ReadSyscall}.
 * @since 0.64.0
 */
final class ReadSyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void rejectsSizeLargerThanTheHeap() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new ReadSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(0), new Data.ToPhi(Integer.MAX_VALUE)
            ),
            "A posix read of more bytes than the heap holds must fail with ExFailure"
        );
    }
}
