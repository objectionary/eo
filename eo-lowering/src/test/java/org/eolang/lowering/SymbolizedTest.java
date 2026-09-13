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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Document;

/**
 * Test case for {@link Symbolized}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class SymbolizedTest {

    @Test
    void seedsVoidsOfFragmentAndOfItsParent(@Mktmp final Path temp) throws IOException {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        final Map<String, String> given = new HashMap<>(2);
        given.put("Φ.foo.f.x", "number");
        given.put("Φ.foo.n", "bool");
        new Symbolized(
            SymbolizedTest.doc(), "Φ.foo.f", new Formas(Collections.emptyMap(), given), symbols
        ).plant();
        MatcherAssert.assertThat(
            "the voids must be seeded with their paths from the fragment, but they werent",
            symbols.rows(),
            Matchers.contains(
                Arrays.asList("S1", "number", "void", "x"),
                Arrays.asList("S2", "bool", "void", "ρ.n")
            )
        );
    }

    @Test
    void leavesReceiverVoidOpen(@Mktmp final Path temp) throws IOException {
        final Document doc = SymbolizedTest.doc();
        new Symbolized(
            doc, "Φ.foo.f", new Formas(Collections.emptyMap(), Collections.emptyMap()),
            new Symbols(temp.resolve("s.tsv"))
        ).plant();
        MatcherAssert.assertThat(
            "the ρ void must stay open for the dispatch to bind, but it was filled",
            new Xml(doc).text(),
            Matchers.containsString("<o base=\"∅\" name=\"ρ\"/>")
        );
    }

    @Test
    void refusesAbsentFragment(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Symbolized(
                SymbolizedTest.doc(), "Φ.foo.z",
                new Formas(Collections.emptyMap(), Collections.emptyMap()),
                new Symbols(temp.resolve("s.tsv"))
            ).plant(),
            "a locator the document lacks cannot be planted, but it was"
        );
    }

    private static Document doc() {
        return new Xnav(
            String.join(
                "",
                "<object><o loc='Φ.foo' name='foo'><o base='∅' name='n' loc='Φ.foo.n'/>",
                "<o loc='Φ.foo.f' name='f'><o base='∅' name='x'/><o base='∅' name='ρ'/>",
                "<o base='ξ.x' name='φ'/></o></o></object>"
            )
        ).element("object").node().getOwnerDocument();
    }


    @Test
    void seedsReceiverOfDataMethodAsItsCarrier(@Mktmp final Path temp) throws IOException {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        new Symbolized(
            SymbolizedTest.data(), "Φ.number.minus",
            new Formas(Collections.emptyMap(), Collections.emptyMap()), symbols
        ).plant();
        MatcherAssert.assertThat(
            "the receiver of a method of a data forma must be seeded as a value of it, but it wasnt",
            symbols.rows(),
            Matchers.hasItem(Arrays.asList("S2", "number", "void", "ρ"))
        );
    }

    @Test
    void fillsVoidOfDataFormaWithPayloadOfMarker(@Mktmp final Path temp) throws IOException {
        final Document doc = SymbolizedTest.data();
        new Symbolized(
            doc, "Φ.number.minus",
            new Formas(Collections.emptyMap(), Collections.emptyMap()),
            new Symbols(temp.resolve("s.tsv"))
        ).plant();
        MatcherAssert.assertThat(
            "the void of the data forma must hold the payload of the marker, but it doesnt",
            new Xml(doc).text(),
            Matchers.containsString(
                "<o base=\"Φ.bytes\" name=\"φ\"><o as=\"φ\"><o name=\"λ\">S2</o></o></o>"
            )
        );
    }

    private static Document data() {
        return new Xnav(
            String.join(
                "",
                "<object><o loc='Φ.number' name='number'><o base='∅' name='φ'/>",
                "<o loc='Φ.number.minus' name='minus'><o base='∅' name='ρ'/><o base='∅' name='x'/>",
                "<o base='ξ.ρ.plus' name='φ'><o as='α0' base='ξ.x'/></o></o></o></object>"
            )
        ).element("object").node().getOwnerDocument();
    }
}
