/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for the diagnostics of {@code socket}.
 *
 * <p>A port that is not an integer is refused by {@code checked-port} with a
 * message of its own. A non-finite port is not an integer either, so it has
 * to reach the same message instead of dying inside the conversion that
 * writes it.</p>
 *
 * <p>The object is taken off {@code Φ} rather than built from its
 * transpiled class, because {@code socket} is written in EO and its class
 * is not there while the sources are being checked.</p>
 *
 * @since 0.64
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOsocketTest {

    @Test
    void namesTheNonFinitePortItRefuses() {
        MatcherAssert.assertThat(
            "a non-finite port must be refused by the message of checked-port, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new PhApplication(
                            Phi.Φ.take("socket"),
                            "address", new Data.ToPhi("127.0.0.1")
                        ),
                        "port", new Data.ToPhi(Double.NaN)
                    ).take("checked-port")
                ).take(),
                "a port of nan must not be accepted"
            ).getMessage(),
            Matchers.containsString("Port must be an integer in 0 to 65535, but was")
        );
    }
}
