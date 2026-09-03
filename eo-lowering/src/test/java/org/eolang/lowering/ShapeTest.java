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
 * Test case for {@link Shape}.
 * @since 0.76.0
 */
final class ShapeTest {

    @Test
    void coversSiteWithPositionalName() {
        MatcherAssert.assertThat(
            "the α name must stand for the resolved one, but it doesnt",
            new Shape(
                "plus",
                "sym:v0",
                Collections.singletonList("x"),
                Collections.singletonList("number:3F-F0-00-00-00-00-00-00")
            ).covers(
                "plus",
                "sym:v0",
                Collections.singletonList(
                    new Binding(
                        "α0", new Literal("number", "3F-F0-00-00-00-00-00-00")
                    )
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsForeignReceiver() {
        MatcherAssert.assertThat(
            "a site of another receiver must stay uncovered, but it doesnt",
            new Shape(
                "size",
                "sym:v0",
                Collections.emptyList(),
                Collections.emptyList()
            ).covers("size", "sym:v1", Collections.emptyList()),
            Matchers.is(false)
        );
    }

    @Test
    void rejectsMisplacedArgument() {
        MatcherAssert.assertThat(
            "an argument out of its position must stay uncovered, but it doesnt",
            new Shape(
                "slice",
                "sym:v0",
                Arrays.asList("start", "len"),
                Arrays.asList("number:11-", "number:22-")
            ).covers(
                "slice",
                "sym:v0",
                Arrays.asList(
                    new Binding("len", new Literal("number", "22-")),
                    new Binding("start", new Literal("number", "11-"))
                )
            ),
            Matchers.is(false)
        );
    }
}
