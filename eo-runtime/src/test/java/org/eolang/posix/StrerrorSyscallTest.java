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
 * Test case for {@link StrerrorSyscall}.
 *
 * @since 0.57.0
 */
final class StrerrorSyscallTest {

    @Test
    void refusesAFractionalErrorNumber() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new StrerrorSyscall(Phi.Φ.take("posix").copy()).make(new Data.ToPhi(2.5)),
            "a fractional error number must fail instead of quietly looking up another error"
        );
    }

    @Test
    void refusesAnErrorNumberBeyondIntRange() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new StrerrorSyscall(Phi.Φ.take("posix").copy()).make(
                new Data.ToPhi(3.0e9)
            ),
            "an error number past the int range must fail instead of saturating"
        );
    }
}
