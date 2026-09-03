/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Reduction}.
 *
 * <p>The tests that run the real binary hold only when it is installed
 * and of the pinned version; a machine without it skips them. Together
 * they also pin the {@code --partial} semantics this module rests on: if
 * an upgrade of phino reshapes what parks, what fires, or how records
 * render, the reductions here settle differently and fail.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ReductionTest {

    @Test
    void reducesChainIntoTwoSteps(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a two-application chain must make two steps, but it didnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='.times'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves(),
            Matchers.hasSize(2)
        );
    }

    @Test
    void answersWithLastStep(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the answer must name the final step, but it doesnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='.times'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().answer(),
            Matchers.equalTo("sym:s2")
        );
    }

    @Test
    void recordsOperandsOfFirstStep(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the first step must take the void and the literal, but it doesnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='.times'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves().get(0).keys(),
            Matchers.contains("sym:v0", "number:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void foldsAllLiteralSubtermIntoOperand(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the product of 2 and 3 must fold into the one step, but it didnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='.times'>",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-08-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves().get(0).keys(),
            Matchers.contains("sym:v0", "number:40-18-00-00-00-00-00-00")
        );
    }

    @Test
    void collapsesIdenticalSitesIntoOneStep(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "two copies of one application must share a step, but they dont",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='.times'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='.times'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves(),
            Matchers.hasSize(2)
        );
    }

    @Test
    void reducesBytesOperation(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the size of symbolic bytes must make one step, but it didnt",
            new Reduction(
                phino,
                new Xnav("<o base='.size'><o base='ξ.b'/></o>").element("o"),
                Collections.singletonMap("b", "bytes"),
                8
            ).protocol().moves().get(0).atom(),
            Matchers.equalTo("L_bytes_size")
        );
    }

    @Test
    void answersComparisonWithBool(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a fragment led by gt must answer a bool, but it doesnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.gt'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().carrier(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void refusesComparisonMidTree(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.gt'>",
                        "<o base='.gt'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            )::protocol,
            "a bool feeding a later step cannot reduce, but it did"
        );
    }

    @Test
    void refusesForeignMethod(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.minus'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            )::protocol,
            "a method the universe does not hold cannot reduce, but it did"
        );
    }

    @Test
    void stopsOnExhaustedBudget(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='.times'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                1
            )::protocol,
            "a chain of two steps cannot settle in one round, but it did"
        );
    }

    @Test
    void answersVoidWithoutSteps(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a fragment answering its own void needs no step, but it took one",
            new Reduction(
                new Phino("phino", 1000, temp),
                new Xnav("<o base='ξ.x'/>").element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().answer(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void refusesUnknownReference(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                new Phino("phino", 1000, temp),
                new Xnav("<o base='ξ.y'/>").element("o"),
                Collections.singletonMap("x", "number"),
                8
            )::protocol,
            "a reference to no void cannot reduce, but it did"
        );
    }

    @Test
    void foldsFragmentWithoutSymbols(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a fragment of literals alone must fold into its value, but it didnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-08-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.emptyMap(),
                8
            ).protocol().answer(),
            Matchers.equalTo("number:40-14-00-00-00-00-00-00")
        );
    }
}
