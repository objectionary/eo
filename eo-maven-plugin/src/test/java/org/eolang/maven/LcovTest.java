/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.Collections;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Lcov}.
 * @since 0.58
 */
final class LcovTest {

    @Test
    void reportsUntouchedLineAsMissed() {
        final long seed = System.nanoTime();
        final Random random = new Random(seed);
        final int line = 1 + random.nextInt(1000);
        MatcherAssert.assertThat(
            String.format("a line nobody touched is not reported as missed, seed: %d", seed),
            new Lcov(
                Collections.singleton(String.format("org.eolang.числò:%d:5", line)),
                Collections.emptyList()
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:org/eolang/числò.eo%nDA:%d,0%nLF:1%nLH:0%nend_of_record%n",
                    line
                )
            )
        );
    }

    @Test
    void countsEveryTouchedPositionOfOneLine() {
        MatcherAssert.assertThat(
            "the hits of two objects sharing a line are not summed into one line record",
            new Lcov(
                Arrays.asList(
                    "org.eolang.number:12:5",
                    "org.eolang.number:12:9",
                    "org.eolang.number:13:1"
                ),
                Arrays.asList("org.eolang.number:12:9", "org.eolang.number:12:5")
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:org/eolang/number.eo%nDA:12,2%nDA:13,0%nLF:2%nLH:1%nend_of_record%n"
                )
            )
        );
    }

    @Test
    void keepsProgramsApartInAlphabeticalOrder() {
        MatcherAssert.assertThat(
            "two programs are not reported as two separate tracefile records, sorted by name",
            new Lcov(
                Arrays.asList("org.eolang.txt.sprintf:3:1", "org.eolang.bytes:7:2"),
                Collections.singleton("org.eolang.bytes:7:2")
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:org/eolang/bytes.eo%nDA:7,1%nLF:1%nLH:1%nend_of_record%n"
                ).concat(
                    String.format(
                        "TN:%nSF:org/eolang/txt/sprintf.eo%nDA:3,0%nLF:1%nLH:0%nend_of_record%n"
                    )
                )
            )
        );
    }
}
