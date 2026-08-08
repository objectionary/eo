/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ReadFuncCall}.
 * @since 0.1
 */
final class ReadFuncCallTest {

    @Test
    void rejectsNegativeSizeBeforeAllocatingTheBuffer() {
        final Phi win = Phi.Φ.take("win32");
        Assertions.assertThrows(
            ExFailure.class,
            () -> new ReadFuncCall(win).make(new Data.ToPhi(0), new Data.ToPhi(-1)),
            "a negative size must fail through the controlled EO failure path, not a raw JVM exception"
        );
    }
}
