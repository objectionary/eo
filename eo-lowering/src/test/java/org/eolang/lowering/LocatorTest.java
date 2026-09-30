/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests of the class {@link Locator}.
 *
 * @since 0.64.0
 */
final class LocatorTest {

    @Test
    void turnsTheLocatorIntoThePathOfItsProtocol() {
        MatcherAssert.assertThat(
            "the protocol must be at the path the locator names, but it isnt",
            new Locator("Φ.bytes.as-hex.a🌵16-3").protocol(),
            Matchers.equalTo(Path.of("bytes", "as-hex", "a🌵16-3.xml"))
        );
    }

    @Test
    void rejectsALocatorOutsideOfPhi() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Locator("Q.number.neg").protocol(),
            "a locator that does not start with Φ must be rejected, but it isnt"
        );
    }
}
