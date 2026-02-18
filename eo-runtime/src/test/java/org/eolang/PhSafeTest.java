/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import EOorg.EOeolang.EOerror;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhSafeTest}.
 *
 * @since 0.36.0
 */
final class PhSafeTest {

    @Test
    void savesLocationAfterCopying() {
        final Phi located = new PhSafe(new Data.ToPhi(0L), "foo", 123, 124, "qwerty", "fqn");
        MatcherAssert.assertThat(
            "saves location",
            located.copy().locator(),
            Matchers.equalTo(located.locator())
        );
    }

    @Test
    void catchesRuntimeException() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new PhSafe(
                new PhDefault() {
                    @Override
                    public byte[] delta() {
                        throw new IllegalArgumentException("oops");
                    }
                }
            ).delta(),
            "PhSafe should catch RuntimeException and rethrow as ExError, but it didn't"
        );
    }

    @Test
    void rendersMultiLayeredErrorCorrectly() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new PhSafe(
                new PhWith(
                    new EOerror(),
                    "message",
                    new Data.ToPhi("oops")
                )
            ).take("foo"),
            "PhSafe should rethrow multi-layered error as ExError, but it didn't"
        );
    }

    @Test
    void throwsExErrorOnTake() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new PhSafe(
                new PhDefault() {
                    @Override
                    public Phi take(final String name) {
                        throw new IllegalArgumentException("intentional error");
                    }
                }
            ).take("foo"),
            "PhSafe should wrap exception in ExError when take() throws, but it didn't"
        );
    }

}
