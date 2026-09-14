/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Element;

/**
 * Test case for {@link Sites}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class SitesTest {

    @Test
    void putsWholeBodyOfFragmentFirst(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the marker that is the whole φ of the fragment must come first, but it doesnt",
            new ArrayList<>(
                new Sites(
                    SitesTest.fragment(
                        String.format(
                            "<o name='h'>%s</o>%s",
                            SitesTest.number("name='x'", "S3"),
                            SitesTest.number("name='φ'", "S3")
                        )
                    ),
                    SitesTest.sum(temp)
                ).all().keySet()
            ).get(0).getAttribute("name"),
            Matchers.equalTo("φ")
        );
    }

    @Test
    void climbsToCarrierAroundMarker(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the site of a number marker must be the number around it, but it isnt",
            new ArrayList<>(
                new Sites(
                    SitesTest.fragment(SitesTest.number("name='φ'", "S3")), SitesTest.sum(temp)
                ).all().keySet()
            ).get(0).getAttribute("base"),
            Matchers.equalTo("Φ.number")
        );
    }

    @Test
    void standsForTupleOfItsPart(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a marker of a part inside a tuple must stand for the tuple, but it doesnt",
            new Sites(
                SitesTest.fragment(
                    String.format(
                        "<o base='Φ.tuple' name='φ'>%s<o as='head' base='Φ.true'/></o>",
                        SitesTest.number("as='length'", "S2")
                    )
                ),
                SitesTest.table(temp, "S1\ttuple\tvoid\tt", "S2\tnumber\tattr\tsym:S1\tlength")
            ).all(),
            Matchers.hasValue("S1")
        );
    }

    private static String number(final String attr, final String sym) {
        return String.format(
            "<o %s base='Φ.number'><o as='φ' base='Φ.bytes'><o as='φ'><o name='λ'>%s</o></o></o></o>",
            attr, sym
        );
    }

    private static Element fragment(final String body) {
        return (Element) new Xnav(
            String.format(
                "<object><o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'>%s</o></o></object>",
                body
            )
        ).element("object").element("o").element("o").node();
    }

    private static Table sum(final Path temp) throws IOException {
        return SitesTest.table(
            temp,
            "S1\tnumber\tvoid\ta", "S2\tnumber\tvoid\tb",
            "S3\tnumber\tL_number_plus\tsym:S1\tsym:S2"
        );
    }

    private static Table table(final Path temp, final String... rows) throws IOException {
        final Path file = temp.resolve("symbols.tsv");
        Files.write(
            file,
            String.join(System.lineSeparator(), rows)
                .concat(System.lineSeparator())
                .getBytes(StandardCharsets.UTF_8)
        );
        return new Table(new Symbols(file));
    }
}
