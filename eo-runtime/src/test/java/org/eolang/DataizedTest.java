/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Dataized}.
 *
 * @since 0.22
 */
final class DataizedTest {

    // Exercise the logarithm even when EO test transpilation is disabled.
    // The lowered power ladder must reach its base case (#8561).
    @ParameterizedTest
    @ValueSource(doubles = {1.0, 2.0, 3.0, 20.0, 1.0e300, 1.0e-300})
    @Timeout(30L)
    void finishesLogarithm(final double input) {
        MatcherAssert.assertThat(
            "A logarithm must finish with the expected value rather than exhaust memory",
            new Dataized(new Data.ToPhi(input).take("ln")).asNumber(),
            Matchers.closeTo(Math.log(input), 1.0e-9)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "-1.0, NaN", "0.0, -Infinity", "Infinity, Infinity",
        "-Infinity, NaN", "NaN, NaN"
    })
    void keepsLogarithmLimitingCases(final double input, final double expected) {
        MatcherAssert.assertThat(
            "Logarithm limiting cases must return before evaluating the power ladder",
            new Dataized(new Data.ToPhi(input).take("ln")).asNumber(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void failsWithLocationThroughPhSafe() {
        MatcherAssert.assertThat(
            "failure keeps the location of the offending object and its cause",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(
                    new PhSafe(
                        new PhDispatch(
                            new PhDefault() {
                                @Override
                                public Phi take(final String name) {
                                    throw new IllegalStateException("intentional error");
                                }
                            },
                            "xyz"
                        ),
                        "foo.bar", 0, 0
                    )
                ).take(),
                "dataization was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("at foo.bar:0:0"),
                Matchers.containsString("intentional error")
            )
        );
    }

    @Test
    void failsWhenForcingTerminated() {
        MatcherAssert.assertThat(
            "forcing a ⊥ fails with the cause it carries",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new PhTerminator(new Data.ToPhi("boom"))).take(),
                "forcing a ⊥ was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("boom")
        );
    }

    @Test
    void failsWhenTypeIsUnknown() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(new Data.ToPhi(0L)).take(Object.class),
            "requesting an unsupported type was expected to fail with ExFailure"
        );
    }

    @Test
    void refusesAByteThatIsNeitherTrueNorFalse() {
        MatcherAssert.assertThat(
            "a one byte datum that is not 00- or FF- must be refused, not read as false",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new PhDefault(new byte[] {(byte) 0x01})).asBool(),
                "dataizing 01- as boolean was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("only 00- and FF- are booleans")
        );
    }

    @Test
    void reportsActualLengthWhenBoolIsEmpty() {
        MatcherAssert.assertThat(
            "the message must report the true (zero) length, not claim it's over one",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new PhDefault(new byte[0])).asBool(),
                "dataizing empty bytes as boolean was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("length 0")
        );
    }

    @Test
    void refusesBytesThatAreNotValidText() {
        MatcherAssert.assertThat(
            "bytes that are not UTF-8 must be refused, not replaced with U+FFFD",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new Data.ToPhi(new byte[]{(byte) 0xFF})).asString(),
                "a lone FF byte was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("not valid UTF-8")
        );
    }

    @Test
    void refusesHalfOfAMultiByteCharacter() {
        MatcherAssert.assertThat(
            "a sequence that merely stops early must be refused too, not padded with U+FFFD",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(new Data.ToPhi(new byte[]{(byte) 0xD0})).asString(),
                "the first half of a two-byte character was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("not valid UTF-8")
        );
    }

    @Test
    void readsAMultiByteCharacter() {
        MatcherAssert.assertThat(
            "a whole multi-byte character must still be read as it is",
            new Dataized(new Data.ToPhi("привет")).asString(),
            Matchers.equalTo("привет")
        );
    }
}
