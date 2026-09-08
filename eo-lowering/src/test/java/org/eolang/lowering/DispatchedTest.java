/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Dispatched}.
 *
 * <p>The records here are written by hand, the way phino writes them
 * for a parked marker, and the arguments of the sites are values
 * already, so the tests need no binary: they pin how a record meets the
 * tree and what step it leaves behind.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class DispatchedTest {

    @Test
    void mintsStepOfRecordedMethod(@Mktmp final Path temp) throws Exception {
        final List<Step> steps = new ArrayList<>(0);
        DispatchedTest.dispatched(temp).applied(
            DispatchedTest.site("minus", ""), DispatchedTest.record("6D-69-6E-75-73", ""), steps
        );
        MatcherAssert.assertThat(
            "the record must mint a step calling the method over the keys, but it doesnt",
            String.join(" ", steps.get(0).atom(), String.join(" ", steps.get(0).keys())),
            Matchers.equalTo(".minus sym:v0 number:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void holdsObjectWhenFormaUnwitnessed(@Mktmp final Path temp) throws Exception {
        final List<Step> steps = new ArrayList<>(0);
        DispatchedTest.dispatched(temp).applied(
            DispatchedTest.site("minus", ""), DispatchedTest.record("6D-69-6E-75-73", ""), steps
        );
        MatcherAssert.assertThat(
            "a call the tables say nothing about must answer an object, but it doesnt",
            steps.get(0).forma(),
            Matchers.equalTo("object")
        );
    }

    @Test
    void readsWitnessedForma(@Mktmp final Path temp) throws Exception {
        final List<Step> steps = new ArrayList<>(0);
        DispatchedTest.dispatched(temp).applied(
            DispatchedTest.site("minus", "number"),
            DispatchedTest.record("6D-69-6E-75-73", "6E-75-6D-62-65-72"), steps
        );
        MatcherAssert.assertThat(
            "the forma the marker carries must type the step, but it doesnt",
            steps.get(0).forma(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void swapsSiteWithSymbolOfStep(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the site must give way to the symbol of the step, but it didnt",
            DispatchedTest.dispatched(temp).applied(
                DispatchedTest.site("minus", ""),
                DispatchedTest.record("6D-69-6E-75-73", ""),
                new ArrayList<>(0)
            ).get().key(),
            Matchers.equalTo("sym:s1")
        );
    }

    @Test
    void leavesTreeOfForeignMethod(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a record of a method the tree does not dispatch must leave it alone, but it didnt",
            DispatchedTest.dispatched(temp).applied(
                DispatchedTest.site("minus", ""),
                DispatchedTest.record("6E-65-67", ""),
                new ArrayList<>(0)
            ).isPresent(),
            Matchers.is(false)
        );
    }

    @Test
    void refusesLazyMethod(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a method lazy in an argument cannot be called strictly, but it was",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> DispatchedTest.dispatched(temp).applied(
                    DispatchedTest.site("and", ""),
                    DispatchedTest.record("61-6E-64", ""),
                    new ArrayList<>(0)
                ),
                "a dispatch of 'and' minted a step, but it must not"
            ).getMessage(),
            Matchers.containsString("lazy in an argument")
        );
    }

    private static Dispatched dispatched(final Path temp) {
        final Map<String, String> voids = Collections.singletonMap("x", "number");
        return new Dispatched(
            new Reduction(
                new Phino("phino", 1000, temp),
                new Xnav("<o base='ξ.x'/>").element("o"),
                voids,
                8
            ),
            new Minted(voids)
        );
    }

    private static Term site(final String method, final String forma) {
        return new Site(
            method,
            new Symbol("v0", "number"),
            Collections.singletonList(
                new Binding("α0", new Literal("number", "40-00-00-00-00-00-00-00"))
            ),
            forma
        );
    }

    private static Evaluation record(final String method, final String forma) {
        String slots = String.format("m🌵 ↦ ⟦ Δ ⤍ %s, ρ ↦ ∅ ⟧", method);
        if (!forma.isEmpty()) {
            slots = String.format("%s, f🌵 ↦ ⟦ Δ ⤍ %s, ρ ↦ ∅ ⟧", slots, forma);
        }
        return new Evaluation(
            String.join(
                "",
                "L_dispatch\t⟦ self ↦ Φ.number( α0 ↦ Φ.bytes( α0 ↦ ⟦ λ ⤍ Sym_v0, ρ ↦ ∅ ⟧ ) ), ",
                slots,
                ", a0 ↦ Φ.number( as-bytes ↦ Φ.bytes( data ↦ ",
                "⟦ Δ ⤍ 40-00-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) ), ρ ↦ ∅ ⟧"
            )
        );
    }
}
