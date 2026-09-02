/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Pieces}.
 * @since 0.70.0
 */
final class PiecesTest {

    @Test
    void walksAChainOfReceiversLeftward() {
        MatcherAssert.assertThat(
            "each link of the chain must get its own word, but they piled onto one",
            PiecesTest.drawn(
                "  first.as-bytes.size",
                Arrays.asList(
                    new Written("Φ.f.d", 16, "", new Answer("Φ.number", 3)),
                    new Written("Φ.f.d.ρ", 16, "", new Answer("Φ.bytes", 3)),
                    new Written("Φ.f.d.ρ.ρ", 16, "", new Answer("Φ.string", 3))
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit[text='first']/told[@where='Φ.string']",
                "/line/bit[text='.as-bytes']/told[@where='Φ.bytes']",
                "/line/bit[text='.size']/told[@where='Φ.number']"
            )
        );
    }

    @Test
    void marksTheCaretAChainIsDispatchedOn() {
        MatcherAssert.assertThat(
            "the caret a walk is taken off must get a word of its own, but it stayed bare",
            PiecesTest.drawn(
                "  ^.walk",
                Arrays.asList(
                    new Written("Φ.w.α1", 3, "", new Answer("Φ.number", 3)),
                    new Written("Φ.w.α1.ρ", 3, "ρ", new Answer("Φ.string", 3))
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit[text='^']/told[@where='Φ.string']",
                "/line/bit[text='.walk']/told[@where='Φ.number']"
            )
        );
    }

    @Test
    void marksADispatchTakenOffTheCaret() {
        MatcherAssert.assertThat(
            "the step .^ must carry its dot into the mark, but the dot was marked alone",
            PiecesTest.drawn(
                "* ^.^",
                Arrays.asList(
                    new Written("Φ.t.α1", 3, "", new Answer("Φ.number", 3)),
                    new Written("Φ.t.α1.ρ", 3, "ρ", new Answer("Φ.string", 3))
                )
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='.^']/told[@where='Φ.number']")
        );
    }

    @Test
    void walksPastAStepTheSourceNeverWrote() {
        MatcherAssert.assertThat(
            "the steps above an unwritten one must keep their own words, but they piled onto one",
            PiecesTest.drawn(
                "    precise.as-bool.if > end!",
                Arrays.asList(
                    new Written("Φ.p.end", 19, "end", new Answer("Φ.bytes.as-bytes", 3)),
                    new Written("Φ.p.end.ρ.α0", 19, "", new Answer("Φ.bool.if", 1)),
                    new Written("Φ.p.end.ρ.α0.ρ", 19, "", new Answer("Φ.bytes.as-bool", 3)),
                    new Written("Φ.p.end.ρ.α0.ρ.ρ", 19, "", new Answer("Φ.string", 3))
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit[text='precise']/told[@where='Φ.string']",
                "/line/bit[text='.as-bool']/told[@where='Φ.bytes.as-bool']",
                "/line/bit[text='.if']/told[@label='end']"
            )
        );
    }

    @Test
    void keepsTextThatNoObjectClaims() {
        MatcherAssert.assertThat(
            "the brackets around a void are the author's text and must survive, but they didnt",
            PiecesTest.drawn(
                "[if] > bool",
                Collections.singletonList(
                    new Written("Φ.bool.if", 1, "if", new Answer("Φ.bool.if", 1))
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit[.='[']",
                "/line/bit[text='if'][@band='rooted']",
                "/line/bit[.='] > bool']"
            )
        );
    }

    @Test
    void sharesOneMarkBetweenObjectsWrittenOnce() {
        MatcherAssert.assertThat(
            "a literal and the bytes it carries are one word and must share a mark, but didnt",
            PiecesTest.drawn(
                "  plus 42",
                Arrays.asList(
                    new Written("Φ.p.α0", 7, "", new Answer("Φ.number", 3)),
                    new Written("Φ.p.α0.α0", 7, "", new Answer("Φ.bytes", 4))
                )
            ),
            XhtmlMatchers.hasXPath("/line/bit[text='42'][count(told)=2]")
        );
    }

    @Test
    void callsAnUnwrittenNameByWords() {
        MatcherAssert.assertThat(
            "a rho is written nowhere and must be said in words, but its glyph came through",
            PiecesTest.drawn(
                "  oak",
                Collections.singletonList(
                    new Written("Φ.grove.oak.ρ", 2, "ρ", new Answer("Φ.grove", 3))
                )
            ),
            XhtmlMatchers.hasXPath("/line/bit/told[@label='what it is dispatched on (ρ)']")
        );
    }

    @Test
    void saysWhatCallersWereSeenPassing() {
        MatcherAssert.assertThat(
            "an amber mark must say what turned up in the void, but it didnt",
            PiecesTest.drawn(
                "[if] > bool",
                Collections.singletonList(
                    new Written(
                        "Φ.bool.if", 1, "if",
                        new Answer(
                            "Φ.bool.if", 1,
                            Arrays.asList(new Ref("Φ.true"), new Ref("Φ.false"))
                        )
                    )
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/line/bit/told/seen/ref[@loc='Φ.true']",
                "/line/bit/told/seen/ref[@loc='Φ.false']"
            )
        );
    }

    private static String drawn(final String line, final Collection<Written> written) {
        return new Xembler(
            new Directives()
                .add("line")
                .append(new Pieces(line, written).directives())
                .up()
        ).xmlQuietly();
    }
}
