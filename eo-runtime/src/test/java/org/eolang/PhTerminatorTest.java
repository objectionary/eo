/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhTerminator}.
 * @since 0.73.1
 */
final class PhTerminatorTest {

    @Test
    void failsWhenDataized() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(new PhTerminator()).take(),
            "dataizing the bottom object must abort instead of returning data"
        );
    }

    @Test
    void failsWhenDispatched() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhTerminator().take("any"),
            "dispatching an attribute on the bottom object must abort"
        );
    }

    @Test
    void failsWhenCopied() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhTerminator().copy(),
            "copying the bottom object must abort"
        );
    }
}
