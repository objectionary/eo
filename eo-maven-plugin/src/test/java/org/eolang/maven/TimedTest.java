/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Timed}.
 * @since 0.61.0
 */
final class TimedTest {

    @Test
    void executesWrappedStep() {
        final AtomicBoolean executed = new AtomicBoolean(false);
        Assertions.assertDoesNotThrow(
            () -> new Timed(() -> executed.set(true)).exec(),
            "Timed must delegate execution to the wrapped step"
        );
        Assertions.assertTrue(
            executed.get(),
            "The wrapped step must have been executed"
        );
    }

    @Test
    void propagatesException() {
        Assertions.assertThrows(
            IOException.class,
            () -> new Timed(
                () -> {
                    throw new IOException("simulated failure");
                }
            ).exec(),
            "Timed must propagate IOException thrown by the wrapped step"
        );
    }
}
