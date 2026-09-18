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
 * Test case for {@link GetenvSyscall}.
 *
 * @since 0.57.0
 */
final class GetenvSyscallTest {

    @Test
    void refusesAnEnvironmentNameWithNul() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new GetenvSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(String.join(String.valueOf((char) 0), "PATH", "nope"))
            ),
            "a name whose NUL would make getenv read only its prefix must fail, not answer for that prefix"
        );
    }
}
