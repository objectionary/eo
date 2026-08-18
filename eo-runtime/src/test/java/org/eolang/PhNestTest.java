/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhNest}.
 * @since 0.62
 */
final class PhNestTest {

    @Test
    void refusesPackageThatWasNeverTranspiled() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new PhNest("Φ.org.eolang", new Silent()).take("dummy"),
            "A Java package carrying no @XmirPackage must not pass for an EO package, but it did"
        );
    }
}
