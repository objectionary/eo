/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Table}.
 *
 * <p>The tables here are written by hand, so the tests need no phino:
 * they pin the Java that the rows of one morph turn into, which is what
 * lands in the {@code lambda()} of a generated atom class.</p>
 *
 * @since 0.76.0
 */
final class TableTest {

    @Test
    void rendersSquaredDistanceFromItsRows() {
        final Map<String, String> voids = new LinkedHashMap<>(0);
        voids.put("α", "number");
        voids.put("β", "number");
        MatcherAssert.assertThat(
            "the rows of a squared distance are not the Java of its atom",
            new JavaAtom(
                new Table(
                    Arrays.asList(
                        "S1\tnumber\tvoid\tα",
                        "S2\tnumber\tvoid\tβ",
                        "S3\tnumber\tL_number_times\tsym:S2\thex:BF-F0-00-00-00-00-00-00",
                        "S4\tnumber\tL_number_plus\tsym:S1\tsym:S3",
                        "S7\tnumber\tL_number_times\tsym:S4\tsym:S4"
                    )
                ).protocol(),
                voids
            ).text(),
            Matchers.stringContainsInOrder(
                "final double v0 = new Dataized(this.take(\"α\")).asNumber();",
                "final double s3 = v1 * Double.longBitsToDouble(0xBFF0000000000000L);",
                "final double s4 = v0 + s3;",
                "return new Data.ToPhi(s7);"
            )
        );
    }

    @Test
    void refusesTheSymbolNoRowMints() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Table(
                Collections.singletonList("S2\tnumber\tL_number_plus\tsym:S1\tsym:S1")
            )::protocol,
            "a row reading a symbol nothing minted is not refused"
        );
    }
}
