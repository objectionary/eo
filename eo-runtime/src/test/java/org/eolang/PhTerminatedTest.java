/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhTerminated}.
 * @since 0.73.1
 */
final class PhTerminatedTest {

    @Test
    void failsWhenDataized() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(new PhTerminated()).take(),
            "dataizing the bottom object must abort instead of returning data"
        );
    }
}
