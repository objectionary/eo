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
                        new Application(
                            "s1", "L_number_plus",
                            Arrays.asList("sym:v0", "number:40-00-00-00-00-00-00-00")
                        ),
                        new Application("s2", "L_number_times", Arrays.asList("sym:s1", "sym:v0"))
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
                        new Application(
                            "s1", "L_bytes_and",
                            Arrays.asList("sym:v0", "bytes:1F-")
                        ),
                        new Application("s2", "L_bytes_size", Collections.singletonList("sym:s1")),
                        new Application(
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
    void readsBoolVoid() {
        MatcherAssert.assertThat(
            "a bool void must be read through asBool, but it isnt",
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Application("s1", "L_bytes_eq", Arrays.asList("sym:v0", "bool:FF-"))
                    ),
                    "sym:s1",
                    "bool"
                ),
                Collections.singletonMap("f", "bool")
            ).text(),
            Matchers.equalTo(
                String.join(
                    System.lineSeparator(),
                    "        final boolean v0 = new Dataized(this.take(\"f\")).asBool();",
                    "        final boolean s1 = v0 == true;",
                    "        return new Data.ToPhi(s1);"
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
                        new Application(
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
    void rendersNumberEqualityAsBitComparison() {
        final Map<String, String> voids = new LinkedHashMap<>();
        voids.put("x", "number");
        voids.put("y", "number");
        MatcherAssert.assertThat(
            "equality over numbers must compare their raw bits, but it doesnt",
            new JavaAtom(
                new Protocol(
                    Arrays.asList(
                        new Application("s1", "L_number_div", Arrays.asList("sym:v0", "sym:v1")),
                        new Application("s2", "L_bytes_eq", Arrays.asList("sym:s1", "sym:v0"))
                    ),
                    "sym:s2",
                    "bool"
                ),
                voids
            ).text(),
            Matchers.containsString(
                String.format(
                    "final boolean s2 = %s == %s;",
                    "Double.doubleToRawLongBits(s1)",
                    "Double.doubleToRawLongBits(v0)"
                )
            )
        );
    }

    @Test
    void readsStringVoidAsItsBytes() {
        MatcherAssert.assertThat(
            "a string void must be dataized into a byte array, but it isnt",
            new JavaAtom(
                new Protocol(
                    Arrays.asList(
                        new Application(
                            "s1", "L_bytes_concat",
                            Arrays.asList("sym:v0", "string:21-")
                        ),
                        new Application("s2", "L_bytes_size", Collections.singletonList("sym:s1"))
                    ),
                    "sym:s2",
                    "number"
                ),
                Collections.singletonMap("t", "string")
            ).text(),
            Matchers.containsString(
                "final byte[] v0 = new Dataized(this.take(\"t\")).take();"
            )
        );
    }

    @Test
    void rendersStringEqualityThroughArrays() {
        MatcherAssert.assertThat(
            "equality over a string and a text literal must go through Arrays, but it doesnt",
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Application(
                            "s1", "L_bytes_eq", Arrays.asList("sym:v0", "string:61-62-63")
                        )
                    ),
                    "sym:s1",
                    "bool"
                ),
                Collections.singletonMap("t", "string")
            ).text(),
            Matchers.containsString(
                String.format(
                    "final boolean s1 = java.util.Arrays.equals(v0, %s);",
                    "new byte[] {(byte) 0x61, (byte) 0x62, (byte) 0x63}"
                )
            )
        );
    }

    @Test
    void rendersBytesEqualityThroughArrays() {
        MatcherAssert.assertThat(
            "equality over bytes must go through Arrays, but it doesnt",
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Application("s1", "L_bytes_eq", Arrays.asList("sym:v0", "bytes:1F-2E-"))
                    ),
                    "sym:s1",
                    "bool"
                ),
                Collections.singletonMap("b", "bytes")
            ).text(),
            Matchers.containsString(
                String.format(
                    "final boolean s1 = java.util.Arrays.equals(v0, %s);",
                    "new byte[] {(byte) 0x1F, (byte) 0x2E}"
                )
            )
        );
    }

    @Test
    void rendersForkAsIfElse() {
        MatcherAssert.assertThat(
            "a fork must declare a blank final and assign it in both arms, but it doesnt",
            new JavaAtom(
                new Protocol(
                    Arrays.asList(
                        new Application(
                            "s1", "L_number_gt",
                            Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00")
                        ),
                        new Fork(
                            "s2", "L_bool_if", "sym:s1",
                            new Protocol(
                                Collections.emptyList(), "number:3F-F0-00-00-00-00-00-00", "number"
                            ),
                            new Protocol(
                                Collections.singletonList(
                                    new Application(
                                        "s3", "L_number_times", Arrays.asList("sym:v0", "sym:v0")
                                    )
                                ),
                                "sym:s3",
                                "number"
                            )
                        )
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
                    "        final boolean s1 = v0 > Double.longBitsToDouble(0x3FF0000000000000L);",
                    "        final double s2;",
                    "        if (s1) {",
                    "            s2 = Double.longBitsToDouble(0x3FF0000000000000L);",
                    "        } else {",
                    "            final double s3 = v0 * v0;",
                    "            s2 = s3;",
                    "        }",
                    "        return new Data.ToPhi(s2);"
                )
            )
        );
    }

    @Test
    void readsVoidInsideTheArmThatAloneUsesIt() {
        final Map<String, String> voids = new LinkedHashMap<>();
        voids.put("f", "bool");
        voids.put("y", "number");
        MatcherAssert.assertThat(
            "a void one arm alone reads must be dataized inside that arm, but it isnt",
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Fork(
                            "s1", "L_bool_if", "sym:v0",
                            new Protocol(Collections.emptyList(), "sym:v1", "number"),
                            new Protocol(
                                Collections.emptyList(), "number:00-00-00-00-00-00-00-00", "number"
                            )
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
                    "        final boolean v0 = new Dataized(this.take(\"f\")).asBool();",
                    "        final double s1;",
                    "        if (v0) {",
                    "            final double v1 = new Dataized(this.take(\"y\")).asNumber();",
                    "            s1 = v1;",
                    "        } else {",
                    "            s1 = Double.longBitsToDouble(0x0000000000000000L);",
                    "        }",
                    "        return new Data.ToPhi(s1);"
                )
            )
        );
    }

    @Test
    void readsVoidBothArmsUseAboveTheFork() {
        final Map<String, String> voids = new LinkedHashMap<>();
        voids.put("f", "bool");
        voids.put("y", "number");
        MatcherAssert.assertThat(
            "a void both arms read must be dataized once, above the fork, but it isnt",
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Fork(
                            "s1", "L_bool_if", "sym:v0",
                            new Protocol(Collections.emptyList(), "sym:v1", "number"),
                            new Protocol(
                                Collections.singletonList(
                                    new Application(
                                        "s2", "L_number_times", Arrays.asList("sym:v1", "sym:v1")
                                    )
                                ),
                                "sym:s2",
                                "number"
                            )
                        )
                    ),
                    "sym:s1",
                    "number"
                ),
                voids
            ).text(),
            Matchers.startsWith(
                String.join(
                    System.lineSeparator(),
                    "        final boolean v0 = new Dataized(this.take(\"f\")).asBool();",
                    "        final double v1 = new Dataized(this.take(\"y\")).asNumber();",
                    "        final double s1;",
                    "        if (v0) {",
                    "            s1 = v1;"
                )
            )
        );
    }

    @Test
    void refusesForkOnNumberCondition() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Fork(
                            "s1", "L_bool_if", "sym:v0",
                            new Protocol(Collections.emptyList(), "sym:v0", "number"),
                            new Protocol(Collections.emptyList(), "sym:v0", "number")
                        )
                    ),
                    "sym:s1",
                    "number"
                ),
                Collections.singletonMap("x", "number")
            )::text,
            "a number cannot decide a fork, but one rendered"
        );
    }

    @Test
    void refusesEqualityOverMixedFormas() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Application("s1", "L_bytes_eq", Arrays.asList("sym:v0", "bytes:01-02-"))
                    ),
                    "sym:s1",
                    "bool"
                ),
                Collections.singletonMap("x", "number")
            )::text,
            "equality mixing a number with raw bytes cannot render, but it did"
        );
    }

    @Test
    void refusesOperandOfForeignForma() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Application(
                            "s1", "L_number_plus",
                            Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00")
                        )
                    ),
                    "sym:s1",
                    "number"
                ),
                Collections.singletonMap("b", "bytes")
            )::text,
            "a number operation over a bytes operand cannot render, but it did"
        );
    }

    @Test
    void refusesOperationWithoutRendering() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(
                    Collections.singletonList(
                        new Application(
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
                new Protocol(Collections.emptyList(), "sym:v0", "tuple"),
                Collections.singletonMap("s", "tuple")
            )::text,
            "a void of a foreign forma cannot be read, but it was"
        );
    }

    @Test
    void refusesStringAnswer() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new JavaAtom(
                new Protocol(Collections.emptyList(), "sym:v0", "string"),
                Collections.singletonMap("t", "string")
            )::text,
            "a byte array handed over as a string would change the forma, but it was"
        );
    }
}
