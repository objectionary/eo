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
 * @since 0.68.0
 */
final class TimedTest {

    @Test
    void executesWrappedStep() throws IOException {
        final AtomicBoolean executed = new AtomicBoolean(false);
        new Timed(
            () -> executed.set(true),
            "test step"
        ).exec();
        Assertions.assertTrue(
            executed.get(),
            "Timed must execute the wrapped step"
        );
    }

    @Test
    void propagatesException() {
        Assertions.assertThrows(
            IOException.class,
            () -> new Timed(
                () -> {
                    throw new IOException("simulated failure");
                },
                "failing step"
            ).exec(),
            "Timed must propagate IOException from the wrapped step"
        );
    }
}