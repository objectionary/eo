/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ExInterrupted}.
 *
 * @since 0.28.3
 */
final class ExInterruptedTest {

    @Test
    void throwsRightException() {
        final EOthrow phi = new EOthrow();
        Assertions.assertThrows(
            ExInterrupted.class,
            () -> new Dataized(phi.take(Phi.PHI)).take(),
            PhCompositeTest.TO_ADD_MESSAGE
        );
    }

    /**
     * Phi object that throw InterruptedException.
     *
     * @since 0.28.3
     */
    private static final class EOthrow extends PhDefault implements Atom {
        @Override
        public Phi lambda() throws Exception {
            throw new InterruptedException();
        }
    }
}
