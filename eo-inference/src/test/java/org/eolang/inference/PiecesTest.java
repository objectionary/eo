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
            XhtmlMatchers.hasXPath("/line/bit/told[@label='the object it sits in (ρ)']")
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
