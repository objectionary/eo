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
 * Test case for {@link ReadSyscall}.
 * @since 0.1
 */
final class ReadSyscallTest {

    @Test
    void rejectsNegativeSizeBeforeAllocatingTheBuffer() {
        final Phi posix = Phi.Φ.take("posix");
        Assertions.assertThrows(
            ExFailure.class,
            () -> new ReadSyscall(posix).make(new Data.ToPhi(0), new Data.ToPhi(-1)),
            "a negative size must fail through the controlled EO failure path, not a raw JVM exception"
        );
    }
}
