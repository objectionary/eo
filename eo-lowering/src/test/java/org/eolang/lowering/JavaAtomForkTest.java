/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for the forks of {@link JavaAtom}.
 *
 * <p>An arm of a fork answers into the local the fork declares, so the
 * answer has to be seen as the forma of that local. These pin the Java
 * text such an arm renders into.</p>
 *
 * @since 0.76.0
 */
final class JavaAtomForkTest {

    @Test
    void viewsTheAnswerOfAnArmAsTheFormaOfTheFork() {
        MatcherAssert.assertThat(
            "an arm answering a view of a local must be assigned as bytes, but it wasnt",
            new JavaAtom(
                new Protocol(
                    Arrays.asList(
                        new Application(
                            "s0", "L_number_equal",
                            Arrays.asList("sym:v0", "number:00-00-00-00-00-00-00-00")
                        ),
                        new Fork(
                            "s2", "L_bool_if", "sym:s0",
                            new Protocol(
                                Collections.singletonList(
                                    new Application(
                                        "s1", "L_number_plus",
                                        Arrays.asList(
                                            "sym:v0", "number:3F-F0-00-00-00-00-00-00"
                                        )
                                    )
                                ),
                                "sym:s1", "bytes"
                            ),
                            new Protocol(
                                Collections.singletonList(
                                    new Application(
                                        "s3", "L_number_times",
                                        Arrays.asList(
                                            "sym:v0", "number:40-00-00-00-00-00-00-00"
                                        )
                                    )
                                ),
                                "sym:s3", "bytes"
                            )
                        )
                    ),
                    "sym:s2", "bytes"
                ),
                Collections.singletonMap("x", "number")
            ).text(),
            Matchers.stringContainsInOrder(
                "final byte[] s2;",
                "s2 = java.nio.ByteBuffer.allocate(8)",
                "s2 = java.nio.ByteBuffer.allocate(8)",
                "return new Data.ToPhi(s2);"
            )
        );
    }
}
