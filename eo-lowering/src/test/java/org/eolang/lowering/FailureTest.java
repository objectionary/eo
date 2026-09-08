/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Failure}.
 *
 * <p>A reason that is already a value settles without phino, so the
 * tests here need no binary: they pin what a failure hands the
 * protocol, and what it refuses.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class FailureTest {

    @Test
    void failsWithLiteralReason(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the protocol must fail with the key of the reason, but it doesnt",
            FailureTest.failure(temp).protocol(
                new Fail(new Literal("string", "6F-70-73")), new ArrayList<>(0)
            ).reason(),
            Matchers.equalTo("string:6F-70-73")
        );
    }

    @Test
    void failsWithStringVoid(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a string void must stand as the reason as it is, but it doesnt",
            FailureTest.failure(temp).protocol(
                new Fail(new Symbol("v0", "string")), new ArrayList<>(0)
            ).reason(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void answersNothing(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a failing protocol must name no carrier, but it does",
            FailureTest.failure(temp).protocol(
                new Fail(new Literal("string", "6F-70-73")), new ArrayList<>(0)
            ).carrier(),
            Matchers.emptyString()
        );
    }

    @Test
    void refusesNumberAsReason(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a reason that is no string cannot be the message, but it was taken",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> FailureTest.failure(temp).protocol(
                    new Fail(new Literal("number", "40-00-00-00-00-00-00-00")),
                    new ArrayList<>(0)
                ),
                "a number reason settled, but it must not"
            ).getMessage(),
            Matchers.containsString("must be a string")
        );
    }

    @Test
    void refusesTerminatorAsReason(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> FailureTest.failure(temp).protocol(
                new Fail(new Fail(new Literal("string", "6F-70-73"))),
                new ArrayList<>(0)
            ),
            "a terminator failing with a terminator settled, but it must not"
        );
    }

    private static Failure failure(final Path temp) {
        final Map<String, String> voids = Collections.singletonMap("t", "string");
        return new Failure(
            new Reduction(
                new Phino("phino", 1000, temp),
                new Xnav("<o base='ξ.t'/>").element("o"),
                voids,
                8
            ),
            new Minted(voids)
        );
    }
}
