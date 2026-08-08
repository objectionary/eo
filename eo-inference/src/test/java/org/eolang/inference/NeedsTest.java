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
 * Test case for {@link Needs}.
 * @since 0.68.0
 */
@ExtendWith(MktmpResolver.class)
final class NeedsTest {

    @Test
    void recordsAttributeTakenFromReceiver(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the object a dispatch takes from must be asked for that attribute, but it isnt",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.app.inc.φ' base='.foo'>",
                    "<o loc='Φ.app.inc.φ.ρ' base='.next'>",
                    "<o loc='Φ.app.inc.φ.ρ.ρ' base='ξ.x'/>",
                    "</o></o></object>"
                )
            ),
            XhtmlMatchers.hasXPath(
                "/needs/type[@id='Φ.app.inc.φ.ρ.ρ']/attr[@name='next' and @type='Φ.app.inc.φ.ρ']"
            )
        );
    }

    @Test
    void recordsEveryStepOfChain(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a dispatch on a dispatch must ask the inner one for the attribute, but it doesnt",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.app.inc.φ' base='.foo'>",
                    "<o loc='Φ.app.inc.φ.ρ' base='.next'>",
                    "<o loc='Φ.app.inc.φ.ρ.ρ' base='ξ.x'/>",
                    "</o></o></object>"
                )
            ),
            XhtmlMatchers.hasXPath(
                "/needs/type[@id='Φ.app.inc.φ.ρ']/attr[@name='foo' and @type='Φ.app.inc.φ']"
            )
        );
    }

    @Test
    void asksReceiverAndNotArgument(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the attribute is taken from the receiver, not from an argument, but it wasnt",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.sum' base='.plus'>",
                    "<o loc='Φ.sum.ρ' base='ξ.x'/>",
                    "<o loc='Φ.sum.α0' as='α0' base='Φ.number'/>",
                    "</o></object>"
                )
            ),
            XhtmlMatchers.hasXPath("/needs/type[@id='Φ.sum.ρ']/attr[@name='plus']")
        );
    }

    @Test
    void skipsReference(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a reference takes nothing from anybody, so it must ask for nothing, but it asked",
            this.table(temp, "<object><o loc='Φ.pot' base='ξ.jar'/></object>"),
            XhtmlMatchers.hasXPath("/needs[not(type)]")
        );
    }

    @Test
    void putsNeedsOfManyFilesInOneTable(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the dispatches of every file must land in the same table, but they didnt",
            this.table(
                temp,
                String.join(
                    "",
                    "<object><o loc='Φ.jar.φ' base='.lid'>",
                    "<o loc='Φ.jar.φ.ρ' base='ξ.jar'/></o></object>"
                ),
                String.join(
                    "",
                    "<object><o loc='Φ.pot.φ' base='.handle'>",
                    "<o loc='Φ.pot.φ.ρ' base='ξ.pot'/></o></object>"
                )
            ),
            XhtmlMatchers.hasXPath(
                "/needs[type[@id='Φ.jar.φ.ρ'] and type[@id='Φ.pot.φ.ρ']]"
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
        new Needs().follow(dir, temp.resolve("tables"));
        return new XMLDocument(temp.resolve("tables").resolve("needs.xml"));
    }
}
