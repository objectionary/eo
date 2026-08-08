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
 * Test case for {@link Links}.
 * @since 0.68.0
 */
@ExtendWith(MktmpResolver.class)
final class LinksTest {

    @Test
    void linksReferenceToFormationItPointsAt(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a reference must be known as a copy of the formation it names, but it isnt",
            this.app(temp),
            XhtmlMatchers.hasXPath("/links/type[@id='Φ.app.φ.α0' and @copy='Φ.app.t']")
        );
    }

    @Test
    void linksReferenceToVoidItPointsAt(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a reference to a void must be known as a copy of it, but it isnt",
            this.app(temp),
            XhtmlMatchers.hasXPath(
                "/links/type[@id='Φ.app.inc.φ.ρ.ρ' and @copy='Φ.app.inc.x']"
            )
        );
    }

    @Test
    void looksOutwardsForName(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a name unknown nearby must be looked for further out, but it wasnt",
            this.app(temp),
            XhtmlMatchers.hasXPath("/links/type[@id='Φ.app.φ' and @copy='Φ.app.inc']")
        );
    }

    @Test
    void linksReferenceToRootObject(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a Φ-rooted reference must be known as a copy of that object, but it isnt",
            this.table(
                temp,
                "<object><o loc='Φ.two' name='two'><o loc='Φ.two.φ' name='φ' base='Φ.number'/></o></object>",
                "<object><o loc='Φ.number' name='number'/></object>"
            ),
            XhtmlMatchers.hasXPath("/links/type[@id='Φ.two.φ' and @copy='Φ.number']")
        );
    }

    @Test
    void staysSilentOnNameItCannotFind(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a name that resolves to nothing must be left alone, but a row was invented",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.jar' name='jar'>",
                    "<o loc='Φ.jar.φ' name='φ' base='ξ.nowhere'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/links[not(type)]")
        );
    }

    /**
     * The table of the note's program, which references a formation, a void
     * and a name bound further out.
     * @param temp The temporary directory to work in
     * @return The table
     * @throws IOException If a file cannot be read or written
     */
    private XML app(final Path temp) throws IOException {
        return this.table(
            temp,
            String.join(
            "",
                "<object><o loc='Φ.app' name='app'>",
                "<o loc='Φ.app.φ' name='φ' base='ξ.inc'>",
                "<o loc='Φ.app.φ.α0' as='α0' base='ξ.t'/>",
                "</o>",
                "<o loc='Φ.app.inc' name='inc'>",
                "<o loc='Φ.app.inc.x' name='x' base='∅'/>",
                "<o loc='Φ.app.inc.φ' name='φ' base='.foo'>",
                "<o loc='Φ.app.inc.φ.ρ' base='.next'>",
                "<o loc='Φ.app.inc.φ.ρ.ρ' base='ξ.x'/>",
                "</o></o></o>",
                "<o loc='Φ.app.t' name='t'>",
                "<o loc='Φ.app.t.next' name='next'/>",
                "</o></o></object>"
            )
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
        new Links().follow(dir, temp.resolve("tables"));
        return new XMLDocument(temp.resolve("tables").resolve("links.xml"));
    }
}
