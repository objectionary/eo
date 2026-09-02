/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Emissions}.
 * @since 0.1
 */
final class EmissionsTest {

    @Test
    void acceptsPlainName() {
        Assertions.assertDoesNotThrow(
            () -> Emissions.validParam("args", 1, 4),
            "a plain void parameter name was rejected"
        );
    }

    @Test
    void rejectsNameWithTrailingEllipsis() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Emissions.validParam("args...", 1, 4),
            "a void parameter name ending with ... was accepted"
        );
    }

    @Test
    void rejectsNameWithPartialTrailingDots() {
        Assertions.assertThrows(
            ParseError.class,
            () -> Emissions.validParam("args..", 1, 4),
            "a void parameter name ending with .. was accepted"
        );
    }
}
