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
import java.util.LinkedHashMap;
import java.util.Map;
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
                ReductionTest.chain(),
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
                ReductionTest.chain(),
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
                ReductionTest.chain(),
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
    void reducesStringThroughItsBytes(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a symbolic string must reach the byte atoms through its own phi, but it didnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.t.size.plus'>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("t", "string"),
                8
            ).protocol().moves().get(0).atom(),
            Matchers.equalTo("L_bytes_size")
        );
    }

    @Test
    void takesStringLiteralAsAnArgument(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a text literal must stand as the operand of the byte atom, but it doesnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.t.concat'>",
                        "<o as='α0' base='Φ.string'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>21-</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("t", "string"),
                8
            ).protocol().moves().get(0).keys(),
            Matchers.contains("sym:v0", "string:21-")
        );
    }

    @Test
    void refusesTextSlicing(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.t.slice'>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>00-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α1' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("t", "string"),
                8
            )::protocol,
            "slicing a string counts characters, not bytes, so it cannot reduce, but it did"
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
    void reducesComparisonMidTree(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a bool standing mid-tree must feed the step after it, but it didnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.eq'>",
                        "<o base='.gt'>",
                        "<o base='ξ.x'/>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>",
                        "<o as='α0' base='Φ.false'/>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves().get(1).keys(),
            Matchers.contains("sym:s1", "bool:00-")
        );
    }

    @Test
    void forksOnSymbolicBool(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a choice on a parked comparison must fork on its symbol, but it doesnt",
            new Reduction(
                phino,
                ReductionTest.choice(
                    ReductionTest.guard(),
                    ReductionTest.number("α0", "40-00-00-00-00-00-00-00"),
                    "<o as='α1' base='ξ.x'/>"
                ),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves().get(1).keys(),
            Matchers.contains("sym:s1")
        );
    }

    @Test
    void numbersArmStepsAfterFork(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the steps of the second arm must count on from the first arm, but they dont",
            new Reduction(
                phino,
                ReductionTest.choice(
                    ReductionTest.guard(),
                    String.format(
                        "<o as='α0' base='.plus'><o base='ξ.x'/>%s</o>",
                        ReductionTest.number("α0", "3F-F0-00-00-00-00-00-00")
                    ),
                    "<o as='α1' base='.times'><o base='ξ.x'/><o as='α0' base='ξ.x'/></o>"
                ),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves().get(1).branches().get(1).moves().get(0).label(),
            Matchers.equalTo("s4")
        );
    }

    @Test
    void picksArmOfLiteralCondition(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a choice decided by data must keep only the arm it picks, but it didnt",
            new Reduction(
                phino,
                ReductionTest.choice(
                    "<o base='Φ.false'/>",
                    "<o as='α0' base='ξ.x'/>",
                    "<o as='α1' base='.times'><o base='ξ.x'/><o as='α0' base='ξ.x'/></o>"
                ),
                Collections.singletonMap("x", "number"),
                8
            ).protocol().moves().get(0).atom(),
            Matchers.equalTo("L_number_times")
        );
    }

    @Test
    void forksOnBoolVoid(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a choice on a bool void must fork on that void, but it doesnt",
            new Reduction(
                phino,
                ReductionTest.choice(
                    "<o base='ξ.f'/>",
                    ReductionTest.number("α0", "3F-F0-00-00-00-00-00-00"),
                    ReductionTest.number("α1", "40-00-00-00-00-00-00-00")
                ),
                Collections.singletonMap("f", "bool"),
                8
            ).protocol().moves().get(0).keys(),
            Matchers.contains("sym:v0")
        );
    }

    @Test
    void refusesForkOfDisagreeingArms(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                ReductionTest.choice(
                    ReductionTest.guard(),
                    "<o as='α0' base='ξ.x'/>",
                    "<o as='α1' base='Φ.true'/>"
                ),
                Collections.singletonMap("x", "number"),
                8
            )::protocol,
            "arms of a number and a bool cannot share one carrier, but they reduced"
        );
    }

    @Test
    void refusesForkWithStuckArm(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                ReductionTest.choice(
                    ReductionTest.guard(),
                    "<o as='α0' base='ξ.x'/>",
                    String.format(
                        "<o as='α1' base='.minus'><o base='ξ.x'/>%s</o>",
                        ReductionTest.number("α0", "3F-F0-00-00-00-00-00-00")
                    )
                ),
                Collections.singletonMap("x", "number"),
                8
            )::protocol,
            "an arm the universe cannot reduce must refuse the whole fork, but it didnt"
        );
    }

    @Test
    void repeatsOnTailCallToItself(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the arm calling the formation again must end in a repeat, but it doesnt",
            new Reduction(
                phino, ReductionTest.countdown(), ReductionTest.pair(), 8, "down"
            ).protocol().moves().get(1).branches().get(1).again(),
            Matchers.contains("sym:s3", "sym:s4")
        );
    }

    @Test
    void answersThroughForkAroundRepeat(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the program must answer with the fork that guards the repeat, but it doesnt",
            new Reduction(
                phino, ReductionTest.countdown(), ReductionTest.pair(), 8, "down"
            ).protocol().answer(),
            Matchers.equalTo("sym:s2")
        );
    }

    @Test
    void refusesCallToItselfOutsideTail(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                phino,
                new Xnav(
                    String.format(
                        "<o base='.plus'><o base='ξ.ρ.f'><o as='α0' base='ξ.x'/></o>%s</o>",
                        ReductionTest.number("α0", "3F-F0-00-00-00-00-00-00")
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8,
                "f"
            )::protocol,
            "a call to itself feeding an operation is no tail call, but it reduced"
        );
    }

    @Test
    void refusesCallToItselfOfWrongArity(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Reduction(
                new Phino("phino", 1000, temp),
                new Xnav("<o base='ξ.ρ.down'><o as='α0' base='ξ.n'/></o>").element("o"),
                ReductionTest.pair(),
                8,
                "down"
            )::protocol,
            "one argument cannot rebind two voids, but it did"
        );
    }

    @Test
    void refusesNumberMethodOnBool(@Mktmp final Path temp) {
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
            "a bool has no number method to answer, but one reduced"
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
                ReductionTest.chain(),
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

    private static Xnav countdown() {
        return ReductionTest.choice(
            String.format(
                "<o base='ξ.n.eq'>%s</o>",
                ReductionTest.number("α0", "00-00-00-00-00-00-00-00")
            ),
            "<o as='α0' base='ξ.acc'/>",
            String.join(
                "",
                "<o as='α1' base='ξ.ρ.down'>",
                "<o as='α0' base='.plus'><o base='ξ.n'/>",
                ReductionTest.number("α0", "BF-F0-00-00-00-00-00-00"),
                "</o>",
                "<o as='α1' base='.times'><o base='ξ.acc'/><o as='α0' base='ξ.n'/></o>",
                "</o>"
            )
        );
    }

    private static Map<String, String> pair() {
        final Map<String, String> out = new LinkedHashMap<>();
        out.put("n", "number");
        out.put("acc", "number");
        return out;
    }

    private static Xnav choice(final String test, final String yes, final String not) {
        return new Xnav(
            String.format("<o base='.if'>%s%s%s</o>", test, yes, not)
        ).element("o");
    }

    private static String guard() {
        return String.format(
            "<o base='.gt'><o base='ξ.x'/>%s</o>",
            ReductionTest.number("α0", "3F-F0-00-00-00-00-00-00")
        );
    }

    private static String number(final String name, final String hex) {
        return String.format(
            "<o as='%s' base='Φ.number'><o as='α0' base='Φ.bytes'><o as='α0'>%s</o></o></o>",
            name, hex
        );
    }

    private static Xnav chain() {
        return new Xnav(
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
        ).element("o");
    }
}
