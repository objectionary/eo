/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Parsed}.
 *
 * <p>The parser rolls a dispatch chain rooted in a reference into the
 * base itself, so {@code b.size.plus 2} arrives as one element with
 * {@code base="ξ.b.size.plus"} and the argument as its only child. The
 * tests here pin how such rolled bases unroll back into sites.</p>
 *
 * @since 0.76.0
 */
final class ParsedTest {

    @Test
    void unrollsRolledDispatchChain() {
        MatcherAssert.assertThat(
            "a rolled chain must unroll into nested sites, but it didnt",
            new Parsed(
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.b.size.plus'>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("b", "bytes")
            ).term().phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧).size.plus",
                    "(α0 ↦ Φ.number(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧)))"
                )
            )
        );
    }

    @Test
    void resolvesBareReference() {
        MatcherAssert.assertThat(
            "a bare reference must become the symbol of its void, but it didnt",
            new Parsed(
                new Xnav("<o base='ξ.x'/>").element("o"),
                Collections.singletonMap("x", "number")
            ).term().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void readsStringLiteralIntoItsCarrier() {
        MatcherAssert.assertThat(
            "a string literal must keep its own carrier around the datum, but it didnt",
            new Parsed(
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.t.eq'>",
                        "<o as='α0' base='Φ.string'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>61-62-63</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("t", "string")
            ).term().phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "Φ.string(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_v0 ⟧)).eq",
                    "(α0 ↦ Φ.string(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ 61-62-63 ⟧)))"
                )
            )
        );
    }

    @Test
    void readsCallToItselfAsRepeat() {
        MatcherAssert.assertThat(
            "a call of the formation to itself must become a repeat, but it didnt",
            new Parsed(
                new Xnav("<o base='ξ.ρ.f'><o as='α0' base='ξ.x'/></o>").element("o"),
                Collections.singletonMap("x", "number"),
                "f"
            ).term().again().get(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void refusesCallThroughRhoToAnother() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Parsed(
                new Xnav("<o base='ξ.ρ.g'><o as='α0' base='ξ.x'/></o>").element("o"),
                Collections.singletonMap("x", "number"),
                "f"
            )::term,
            "a call through ρ to a sibling depends on a context the fragment lacks, but it parsed"
        );
    }

    @Test
    void readsConstAsBytesOfItsTarget() {
        MatcherAssert.assertThat(
            "a const must read as the bytes of what it forces, but it doesnt",
            new Parsed(
                new Xnav(
                    "<o base='.as-bytes'><o base='Φ.dataized'><o base='ξ.x'/></o></o>"
                ).element("o"),
                Collections.singletonMap("x", "number")
            ).term().forma(),
            Matchers.equalTo("bytes")
        );
    }

    @Test
    void keepsKeyOfForcedVoid() {
        MatcherAssert.assertThat(
            "the bytes of a void must still be keyed as the void, but they arent",
            new Parsed(
                new Xnav("<o base='ξ.x.as-bytes'/>").element("o"),
                Collections.singletonMap("x", "number")
            ).term().key(),
            Matchers.equalTo("sym:v0")
        );
    }

    @Test
    void refusesDataizedOfManyTargets() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Parsed(
                new Xnav("<o base='Φ.dataized'><o base='ξ.x'/><o base='ξ.x'/></o>").element("o"),
                Collections.singletonMap("x", "number")
            )::term,
            "a dataized object forcing two targets is malformed, but it parsed"
        );
    }

    @Test
    void appliesHelperFormationToArguments() {
        MatcherAssert.assertThat(
            "a helper with voids must be applied where it is named, but it isnt",
            new Parsed(
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.a🌵3-4'>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                "",
                Collections.singletonMap("a🌵3-4", ParsedTest.scaled())
            ).term().phi(),
            Matchers.stringContainsInOrder(
                "Sym_v0", ".times(", "Δ ⤍ 40-00-00-00-00-00-00-00"
            )
        );
    }

    @Test
    void appliesHelperNamedFromAnotherHelper() {
        final Map<String, Xnav> helpers = new LinkedHashMap<>();
        helpers.put("a🌵3-4", ParsedTest.scaled());
        helpers.put(
            "a🌵7-4",
            new Xnav(
                String.join(
                    "",
                    "<o name='a🌵7-4'><o base='∅' name='ρ'/><o base='∅' name='j'/>",
                    "<o base='ξ.ρ.a🌵3-4' name='φ'><o as='α0' base='ξ.j'/></o></o>"
                )
            ).element("o")
        );
        MatcherAssert.assertThat(
            "a helper naming its sibling through ρ must apply it, but it doesnt",
            new Parsed(
                new Xnav("<o base='ξ.a🌵7-4'><o as='α0' base='ξ.x'/></o>").element("o"),
                Collections.singletonMap("x", "number"),
                "",
                helpers
            ).term().phi(),
            Matchers.stringContainsInOrder("Sym_v0", ".times(", "Sym_v0")
        );
    }

    @Test
    void refusesReachBeyondTheFormation() {
        MatcherAssert.assertThat(
            "a reference past the root through ρ depends on a context the fragment lacks",
            Assertions.assertThrows(
                IllegalStateException.class,
                new Parsed(
                    new Xnav("<o base='ξ.ρ.ρ.x'/>").element("o"),
                    Collections.singletonMap("x", "number"),
                    "f"
                )::term,
                "a reference through two ρ from the root parsed, but it must not"
            ).getMessage(),
            Matchers.containsString("beyond the formation")
        );
    }

    @Test
    void readsHelperInPlace() {
        MatcherAssert.assertThat(
            "a reference to a helper must stand as the helper's own body, but it doesnt",
            new Parsed(
                new Xnav("<o base='ξ.a🌵3-4.plus'><o as='α0' base='ξ.a🌵3-4'/></o>").element("o"),
                Collections.singletonMap("x", "number"),
                "",
                Collections.singletonMap("a🌵3-4", ParsedTest.square())
            ).term().phi(),
            Matchers.stringContainsInOrder(".times(", ".plus(", ".times(")
        );
    }

    @Test
    void refusesHelperReadingItself() {
        MatcherAssert.assertThat(
            "a helper reading itself is a cycle and must be refused as one, but it wasnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                new Parsed(
                    new Xnav("<o base='ξ.a🌵3-4.plus'><o as='α0' base='ξ.x'/></o>").element("o"),
                    Collections.singletonMap("x", "number"),
                    "",
                    Collections.singletonMap(
                        "a🌵3-4",
                        new Xnav(
                            "<o base='ξ.a🌵3-4.times'><o as='α0' base='ξ.x'/></o>"
                        ).element("o")
                    )
                )::term,
                "a helper reading itself never settles, but it parsed"
            ).getMessage(),
            Matchers.containsString("reads itself")
        );
    }

    @Test
    void refusesHelperReadingTheBody() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Parsed(
                new Xnav("<o base='ξ.a🌵3-4.plus'><o as='α0' base='ξ.x'/></o>").element("o"),
                Collections.singletonMap("x", "number"),
                "",
                Collections.singletonMap(
                    "a🌵3-4",
                    new Xnav("<o base='ξ.φ.times'><o as='α0' base='ξ.x'/></o>").element("o")
                )
            )::term,
            "a helper reading the body it is read by is a cycle, but it parsed"
        );
    }

    @Test
    void refusesArgumentsOnBareReference() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Parsed(
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.x'>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number")
            )::term,
            "a void applied to arguments cannot be reduced, but it was"
        );
    }

    private static Xnav scaled() {
        return new Xnav(
            String.join(
                "",
                "<o name='a🌵3-4'><o base='∅' name='ρ'/><o base='∅' name='i'/>",
                "<o base='ξ.ρ.x.times' name='φ'><o as='α0' base='ξ.i'/></o></o>"
            )
        ).element("o");
    }

    private static Xnav square() {
        return new Xnav("<o base='ξ.x.times'><o as='α0' base='ξ.x'/></o>").element("o");
    }
}
