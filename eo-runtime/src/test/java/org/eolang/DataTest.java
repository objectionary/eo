/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link Data}.
 *
 * @since 0.1
 */
final class DataTest {
    @Test
    void comparesVertex() {
        MatcherAssert.assertThat(
            "Hash codes of two Data.ToPhi instances with the same value should differ, but they didn't",
            new Data.ToPhi(42L).hashCode(),
            Matchers.not(
                Matchers.equalTo(
                    new Data.ToPhi(42L).hashCode()
                )
            )
        );
    }

    @ParameterizedTest
    @MethodSource("toPhiData")
    void comparesTwoDatas(final Object value, final String message) {
        MatcherAssert.assertThat(
            message,
            new Data.ToPhi(value),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(value)))
        );
    }

    private static Stream<Arguments> toPhiData() {
        return Stream.of(
            Arguments.of(
                1L, "Data.ToPhi instances with the same long value should differ, but they didn't"
            ),
            Arguments.of(
                "Welcome",
                "Data.ToPhi instances with the same string value should differ, but they didn't"
            ),
            Arguments.of(
                2.18d,
                "Data.ToPhi instances with the same double value should differ, but they didn't"
            ),
            Arguments.of(
                new byte[]{(byte) 0x00, (byte) 0x1f},
                "Data.ToPhi instances with the same byte array value should differ, but they didn't"
            )
        );
    }
}
