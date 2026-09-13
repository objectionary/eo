/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Element;

/**
 * Test case for {@link Slot}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class SlotTest {

    @Test
    void plantsNumberMarker(@Mktmp final Path temp) throws IOException {
        final Element hole = SlotTest.hole();
        new Slot("S1", "number", new Symbols(temp.resolve("s.tsv"))).into(hole);
        MatcherAssert.assertThat(
            "a number void must become a number over bytes over the marker, but it didnt",
            new Xml(hole).text(),
            Matchers.containsString(
                "<o base=\"Φ.number\" name=\"a\"><o as=\"φ\" base=\"Φ.bytes\"><o as=\"φ\"><o name=\"λ\">S1</o></o></o></o>"
            )
        );
    }

    @Test
    void plantsBoolMarkerWithFork(@Mktmp final Path temp) throws IOException {
        final Element hole = SlotTest.hole();
        new Slot("S2", "bool", new Symbols(temp.resolve("s.tsv"))).into(hole);
        MatcherAssert.assertThat(
            "a bool void must hold an if that forks on the marker, but it doesnt",
            new Xml(hole).text(),
            Matchers.containsString(
                "<o name=\"guard\"><o name=\"λ\">S2</o></o><o name=\"λ\">L_fork</o></o></o>"
            )
        );
    }

    @Test
    void mintsPartsOfTupleMarker(@Mktmp final Path temp) throws IOException {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        symbols.record("S1", "tuple", "void", "t");
        new Slot("S1", "tuple", symbols).into(SlotTest.hole());
        MatcherAssert.assertThat(
            "the length of a tuple void must be minted as an attribute row, but it wasnt",
            symbols.rows(),
            Matchers.hasItem(Arrays.asList("S2", "number", "attr", "sym:S1", "length"))
        );
    }

    @Test
    void plantsBareMarkerForObject(@Mktmp final Path temp) throws IOException {
        final Element hole = SlotTest.hole();
        new Slot("S9", "object", new Symbols(temp.resolve("s.tsv"))).into(hole);
        MatcherAssert.assertThat(
            "an untyped void must become a bare formation with the marker, but it didnt",
            new Xml(hole).text(),
            Matchers.containsString("<o name=\"a\"><o name=\"λ\">S9</o></o>")
        );
    }

    private static Element hole() {
        return (Element) new Xnav("<object><o base='∅' name='a'/></object>")
            .element("object").element("o").node();
    }
}
