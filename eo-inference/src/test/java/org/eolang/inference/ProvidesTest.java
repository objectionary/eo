/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Provides}.
 * @since 0.67.0
 */
@ExtendWith(MktmpResolver.class)
final class ProvidesTest {

    @Test
    void listsEveryAttributeOfFormation(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation must give a row to every attribute it binds, but it doesnt",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.kettle' name='kettle'>",
                    "<o loc='Φ.kettle.steam' name='steam'/>",
                    "<o loc='Φ.kettle.whistle' name='whistle'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.kettle' and count(attr)=2]")
        );
    }

    @Test
    void recordsTypeOfAttribute(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the row of an attribute must carry the type of the object bound to it",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.kettle' name='kettle'>",
                    "<o loc='Φ.kettle.steam' name='steam'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.kettle']/attr[@name='steam' and @type='Φ.kettle.steam']"
            )
        );
    }

    @Test
    void marksFormationComplete(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation we have seen entirely must be complete, but it isnt",
            this.table(temp, "<object><o loc='Φ.lid' name='lid'/></object>"),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.lid' and @complete='true']")
        );
    }

    @Test
    void marksVoidAttribute(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void attribute must be marked as such, but it isnt",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.pipe' name='pipe'>",
                    "<o base='∅' loc='Φ.pipe.width' name='width'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/provides/type/attr[@name='width' and @void='true']")
        );
    }

    @Test
    void leavesAtomIncomplete(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an atom hides its body in Java, so its row cannot be complete, but it is",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.tick' name='tick'>",
                    "<o atom='Φ.number' loc='Φ.tick.λ' name='λ'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.tick' and @complete='false']")
        );
    }

    @Test
    void skipsApplications(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an application provides nothing of its own, so it must have no row, but it has",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.lamp' name='lamp'>",
                    "<o base='ξ.bulb' loc='Φ.lamp.φ' name='φ'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/provides[not(type[@id='Φ.lamp.φ'])]")
        );
    }

    @Test
    void skipsData(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "bytes are not a formation, so they must have no row, but they have",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.two' name='two'>",
                    "<o base='Φ.bytes' loc='Φ.two.φ' name='φ'>",
                    "<o as='α0' loc='Φ.two.φ.α0'>02-</o>",
                    "</o></o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/provides[not(type[@id='Φ.two.φ.α0'])]")
        );
    }

    @Test
    void putsRowsOfManyFilesInOneTable(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the formations of every file must land in the same table, but they didnt",
            this.table(
                temp,
                "<object><o loc='Φ.jar' name='jar'/></object>",
                "<object><o loc='Φ.pot' name='pot'/></object>"
            ),
            XhtmlMatchers.hasXPath("/provides[type[@id='Φ.jar'] and type[@id='Φ.pot']]")
        );
    }

    /**
     * The table this clue writes out of the given XMIR.
     * @param temp The temporary directory to work in
     * @param xmirs The XMIR of the program, a document per file
     * @return The table
     * @throws IOException If a file cannot be read or written
     */
    private XML table(final Path temp, final String... xmirs) throws IOException {
        final Path dir = Files.createDirectories(temp.resolve("xmirs"));
        int index = 0;
        for (final String xmir : xmirs) {
            Files.writeString(dir.resolve(String.format("main%d.xmir", index)), xmir);
            index = index + 1;
        }
        new Provides().follow(dir, temp.resolve("tables"));
        return new XMLDocument(temp.resolve("tables").resolve("provides.xml"));
    }
}
