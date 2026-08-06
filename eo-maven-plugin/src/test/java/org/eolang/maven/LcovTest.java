/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Lcov}.
 * @since 0.58
 */
final class LcovTest {

    @Test
    void namesEoSourceOfTouchedProgram() {
        final long seed = System.nanoTime();
        final int line = 1 + new Random(seed).nextInt(1000);
        MatcherAssert.assertThat(
            String.format("the touched program does not name its own .eo source, seed: %d", seed),
            new Lcov(
                Paths.get("src/main/eo"),
                Collections.singleton(String.format("org.eolang.числò:%d:5", line))
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:%s%nDA:%d,1%nLF:1%nLH:1%nend_of_record%n",
                    Paths.get("src/main/eo").resolve("org/eolang/числò.eo"),
                    line
                )
            )
        );
    }

    @Test
    void countsEveryTouchedPositionOfOneLine() {
        MatcherAssert.assertThat(
            "the two objects touched on one line are not counted into one line record",
            new Lcov(
                Paths.get("src/main/eo"),
                Arrays.asList(
                    "org.eolang.number:12:5", "org.eolang.number:12:9", "org.eolang.number:12:5"
                )
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:%s%nDA:12,2%nLF:1%nLH:1%nend_of_record%n",
                    Paths.get("src/main/eo").resolve("org/eolang/number.eo")
                )
            )
        );
    }

    @Test
    void printsNothingWhenNothingWasTouched() {
        MatcherAssert.assertThat(
            "a run that touched nothing is not reported as an empty tracefile",
            new Lcov(Paths.get("src/main/eo"), Collections.emptyList()).toString(),
            Matchers.equalTo("")
        );
    }

    @Test
    void refusesMalformedRecord() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Lcov(
                Paths.get("src/main/eo"), Collections.singleton("org.eolang.числò:4")
            ).toString(),
            "a record that is not program:line:pos is not refused"
        );
    }
}
