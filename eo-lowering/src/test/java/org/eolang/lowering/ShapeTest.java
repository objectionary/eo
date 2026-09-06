/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
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
    void coversTextReceiverRecordedAsBytes() {
        MatcherAssert.assertThat(
            "a bytes receiver must cover the string of the same datum, but it doesnt",
            new Shape(
                "concat",
                "bytes:68-",
                Collections.singletonList("b"),
                Collections.singletonList("sym:v0")
            ).covers(
                "concat",
                "string:68-",
                Collections.singletonList(
                    new Binding("α0", new Symbol("v0", "bytes"))
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsTextReceiverOfAnotherDatum() {
        MatcherAssert.assertThat(
            "a string of another datum must stay uncovered, but it doesnt",
            new Shape(
                "concat",
                "bytes:68-",
                Collections.singletonList("b"),
                Collections.singletonList("sym:v0")
            ).covers(
                "concat",
                "string:69-",
                Collections.singletonList(
                    new Binding("α0", new Symbol("v0", "bytes"))
                )
            ),
            Matchers.is(false)
        );
    }

    @Test
    void coversAnyArgumentWhereBlank() {
        MatcherAssert.assertThat(
            "a blank identity must let any argument through, but it doesnt",
            new Shape(
                "if", "sym:s1", Arrays.asList("t", "f"), Arrays.asList("", "")
            ).covers(
                "if",
                "sym:s1",
                Arrays.asList(
                    new Binding("α0", new Literal("number", "11-")),
                    new Binding(
                        "α1",
                        new Site("size", new Symbol("v0", "bytes"), Collections.emptyList())
                    )
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void coversSiteByItsOwnBindings() {
        final List<Binding> args = Arrays.asList(
            new Binding("α0", new Literal("number", "11-")),
            new Binding(
                "α1",
                new Site("size", new Symbol("v0", "bytes"), Collections.emptyList())
            )
        );
        MatcherAssert.assertThat(
            "a shape taken off a site must cover that very site, but it doesnt",
            new Shape("if", "sym:s1", args).covers("if", "sym:s1", args),
            Matchers.is(true)
        );
    }

    @Test
    void tellsSitesApartByTheirText() {
        MatcherAssert.assertThat(
            "an argument still a site must match only its own text, but it matched another",
            new Shape(
                "if", "sym:s1",
                Collections.singletonList(
                    new Binding(
                        "α0",
                        new Site("size", new Symbol("v0", "bytes"), Collections.emptyList())
                    )
                )
            ).covers(
                "if",
                "sym:s1",
                Collections.singletonList(
                    new Binding(
                        "α0",
                        new Site("size", new Symbol("v1", "bytes"), Collections.emptyList())
                    )
                )
            ),
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
