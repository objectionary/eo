/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Bytes}.
 *
 * @since 0.1
 */
final class BytesTest {

    @Test
    void opensOnEmptyLiteral() {
        MatcherAssert.assertThat(
            "the empty literal `--` must open a BYTES token",
            new Bytes("-- > x", 0).opens(),
            Matchers.is(true)
        );
    }

    @Test
    void opensOnMalformedLiteral() {
        MatcherAssert.assertThat(
            "a dashed hex token must open a BYTES token even when it is malformed",
            new Bytes("0AB- > x", 0).opens(),
            Matchers.is(true)
        );
    }

    @Test
    void staysClosedOnDashedIdentifier() {
        MatcherAssert.assertThat(
            "a dashed identifier must not be mistaken for a BYTES token",
            new Bytes("as-bytes > x", 0).opens(),
            Matchers.is(false)
        );
    }

    @Test
    void staysClosedOnNegativeNumber() {
        MatcherAssert.assertThat(
            "a negative number must not be mistaken for a BYTES token",
            new Bytes("-42 > x", 0).opens(),
            Matchers.is(false)
        );
    }

    @Test
    void readsSingleByte() {
        MatcherAssert.assertThat(
            "a single byte followed by a dash must end before the space",
            new Bytes("0A- > x", 0).end(new Span("0A- > x", 1)),
            Matchers.equalTo(3)
        );
    }

    @Test
    void readsManyBytes() {
        MatcherAssert.assertThat(
            "bytes joined by dashes must be read to the last digit",
            new Bytes("0A-0B-0C > x", 0).end(new Span("0A-0B-0C > x", 1)),
            Matchers.equalTo(8)
        );
    }

    @Test
    void rejectsOddHexRun() {
        MatcherAssert.assertThat(
            "an odd hex run must be named a malformed literal",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Bytes("0AB- > x", 0).end(new Span("0AB- > x", 1))
            ).getMessage(),
            Matchers.equalTo("invalid bytes literal")
        );
    }

    @Test
    void rejectsLowercaseHexDigits() {
        MatcherAssert.assertThat(
            "a lowercase hex digit must be named a malformed literal",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Bytes("0a- > x", 0).end(new Span("0a- > x", 1))
            ).getMessage(),
            Matchers.equalTo("invalid bytes literal")
        );
    }

    @Test
    void rejectsDoubledDashInTheMiddle() {
        MatcherAssert.assertThat(
            "a doubled dash with digits behind it cannot end the literal",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Bytes("0A-0B--0C > x", 0).end(new Span("0A-0B--0C > x", 1))
            ).getMessage(),
            Matchers.equalTo("invalid bytes literal")
        );
    }

    @Test
    void rejectsDanglingContinuationDash() {
        MatcherAssert.assertThat(
            "a dash the token ends on must be named a dangling continuation",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Bytes("0A-0B- > x", 0).end(new Span("0A-0B- > x", 1))
            ).getMessage(),
            Matchers.equalTo("bytes literal ends with a dangling continuation dash")
        );
    }

    @Test
    void detectsOddRunTooShortForAByte() {
        MatcherAssert.assertThat(
            "a single hex digit before a dash must be reported as an odd run",
            new Bytes("A- > x", 0).odd(),
            Matchers.is(true)
        );
    }
}
