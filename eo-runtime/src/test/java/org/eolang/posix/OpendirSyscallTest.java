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

/**
 * Test case for {@link OpendirSyscall}.
 *
 * @since 0.76.0
 */
final class OpendirSyscallTest {

    @Test
    void refusesADirectoryNameWithNul() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new OpendirSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(String.join(String.valueOf((char) 0), "/tmp", "nope"))
            ),
            "a path whose NUL would make opendir read only its prefix must fail, not open that prefix"
        );
    }
}
