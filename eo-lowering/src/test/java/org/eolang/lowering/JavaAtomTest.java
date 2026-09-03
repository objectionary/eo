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
 * Test case for {@link JavaAtom}.
 *
 * <p>The protocols here are built by hand, so the tests need no phino:
 * they pin the exact Java text each shape of protocol renders into,
 * which is what lands in a sidecar file and then, verbatim, in the
 * {@code lambda()} of a generated atom class.</p>
 *
 * @since 0.76.0
 */
final class JavaAtomTest {

    @Test
    void rendersChainOfNumberSteps() {
        MatcherAssert.assertThat(
            "a chain of two steps must render one line each, but it doesnt",
            new JavaAtom(
                new Protocol(
                    Arrays.asList(
                        new Step(
                            "s1", "L_number_plus",
                            Arrays.asList("sym:v0", "number:40-00-00-00-00-00-00-00")
                        ),
                        new Step("s2", "L_number_times", Arrays.asList("sym:s1", "sym:v0"))
                    ),
                    "sym:s2",
                    "number"
                ),
                Collections.singletonMap("x", "number")
            ).text(),
            Matchers.equalTo(
                String.join(
                    System.lineSeparator(),
                    "        final double v0 = new Dataized(this.take(\"x\")).asNumber();",
                    "        final double s1 = v0 + Double.longBitsToDouble(0x4000000000000000L);",
                    "        final double s2 = s1 * v0;",
                    "        return new Data.ToPhi(s2);"
                )
            )
        );
    }

    @Test
    void rendersBytesOperationsThroughPublicApi() {
        MatcherAssert.assertThat(
            "the bytes steps must go through BytesOf and length, but they dont",
            new JavaAtom(
                new Protocol(
                    Arrays.asList(
                        new Step(
                            "s1", "L_bytes_and",
                            Arrays.asList("sym:v0", "bytes:1F-")
                        ),
                        new Step("s2", "L_bytes_size", Collections.singletonList("sym:s1")),
                        new Step(
                            "s3", "L_number_gt",
                            Arrays.asList("sym:s2", "number:40-00-00-00-00-00-00-00")
                        )
                    ),
                    "sym:s3",
                    "bool"
                ),
                Collections.singletonMap("b", "bytes")
            ).text(),
            Matchers.equalTo(
                String.join(
                    System.lineSeparator(),
                    "        final byte[] v0 = new Dataized(this.take(\"b\")).take();",
                    String.format(
                        "        final byte[] s1 = new BytesOf(v0)%s",
                        ".and(new BytesOf(new byte[] {(byte) 0x1F})).take();"
                    ),
                    "        final double s2 = s1.length;",
                    "        final boolean s3 = s2 > Double.longBitsToDouble(0x4000000000000000L);",
                    "        return new Data.ToPhi(s3);"
                )
            )
        );
    }

    @Test
    void answersVoidWithoutSteps() {
        MatcherAssert.assertThat(
            "an identity protocol must read the void and answer it, but it doesnt",
            new JavaAtom(
                new Protocol(Collections.emptyList(), "sym:v0", "number"),
                Collections.singletonMap("x", "number")
            ).text(),
            Matchers.equalTo(
                String.join(
                    System.lineSeparator(),
                    "        final double v0 = new Dataized(this.take(\"x\")).asNumber();",
                    "        return new Data.ToPhi(v0);"
                )
            )
        );
    }

    @Test
    void answersLiteralWithoutReadingVoids() {
        MatcherAssert.assertThat(
            "a constant protocol must not dataize the unused void, but it does",
            new JavaAtom(
                new Protocol(
                    Collections.emptyList(),
                    "number:40-14-00-00-00-00-00-00",
                    "number"
                ),
                Collections.singletonMap("x", "number")
            ).text(),
            Matchers.equalTo(
                "        return new Data.ToPhi(Double.longBitsToDouble(0x4014000000000000L));"
            )
        );
    }

    @Test
    void skipsVoidNoStepReads() {
        final Map<String, String> voids = new LinkedHashMap<>();
        voids.put("x", "number");
        voids.put("y", "number");
        MatcherAssert.assertThat(
            "a void no step reads must not be dataized, but it is",
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Step(
                            "s1", "L_number_plus",
                            Arrays.asList("sym:v1", "number:3F-F0-00-00-00-00-00-00")
                        )
                    ),
                    "sym:s1",
                    "number"
                ),
                voids
            ).text(),
            Matchers.equalTo(
                String.join(
                    System.lineSeparator(),
                    "        final double v1 = new Dataized(this.take(\"y\")).asNumber();",
                    "        final double s1 = v1 + Double.longBitsToDouble(0x3FF0000000000000L);",
                    "        return new Data.ToPhi(s1);"
                )
            )
        );
    }

    @Test
    void refusesOperationWithoutRendering() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Step(
                            "s1", "L_bytes_right",
                            Arrays.asList("sym:v0", "number:40-00-00-00-00-00-00-00")
                        )
                    ),
                    "sym:s1",
                    "bytes"
                ),
                Collections.singletonMap("b", "bytes")
            )::text,
            "an operation without a Java rendering cannot make a body, but it did"
        );
    }

    @Test
    void refusesVoidOfForeignForma() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(Collections.emptyList(), "sym:v0", "string"),
                Collections.singletonMap("s", "string")
            )::text,
            "a void of a foreign forma cannot be read, but it was"
        );
    }
}
