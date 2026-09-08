/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Demanded}.
 * @since 0.69.0
 */
@ExtendWith(MktmpResolver.class)
final class DemandedTest {

    @Test
    void putsWhatIsAskedOfAVoidOnTheVoid(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("inc.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>",
                "<o base='∅' loc='Φ.inc.x' name='x'/>",
                "<o base='.next' loc='Φ.inc.φ' name='φ'>",
                "<o base='ξ.x' loc='Φ.inc.φ.ρ'/></o></o></object>"
            )
        );
        new Demanded(new Resolved(new Clues())).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "the void must remember the name taken from it, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")).nodes(
                "/provides/type[@id='Φ.inc']/attr[@name='x']/demand[@name='next']"
            ),
            Matchers.hasSize(1)
        );
    }

    @Test
    void putsACallMadeOnAVoidOnTheVoid(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("apply.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.apply' name='apply'>",
                "<o base='∅' loc='Φ.apply.x' name='x'/>",
                "<o base='ξ.x' loc='Φ.apply.φ' name='φ'>",
                "<o as='α0' base='Φ.apply' loc='Φ.apply.φ.α0'/></o></o></object>"
            )
        );
        new Demanded(new Resolved(new Clues())).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "the void must remember that it was applied, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")).nodes(
                "//attr[@name='x']/apply[@of='Φ.apply.x' and @place='0']/ref[@loc='Φ.apply']"
            ),
            Matchers.hasSize(1)
        );
    }

    @Test
    void putsACallMadeOnANameRootedAtAVoidOnTheVoid(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("inc.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>",
                "<o base='∅' loc='Φ.inc.x' name='x'/>",
                "<o base='.plus' loc='Φ.inc.φ' name='φ'>",
                "<o base='ξ.x' loc='Φ.inc.φ.ρ'/>",
                "<o as='α0' base='Φ.inc' loc='Φ.inc.φ.α0'/></o></o></object>"
            )
        );
        new Demanded(new Resolved(new Clues())).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "the void must remember the call made a step out of it, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")).nodes(
                "//attr[@name='x']/apply[@of='Φ.inc.x.plus' and @place='0']"
            ),
            Matchers.hasSize(1)
        );
    }

    @Test
    void leavesAVoidNobodyAsksOfAlone(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("pipe.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.pipe' name='pipe'>",
                "<o base='∅' loc='Φ.pipe.x' name='x'/>",
                "<o base='ξ.x' loc='Φ.pipe.φ' name='φ'/></o></object>"
            )
        );
        new Demanded(new Resolved(new Clues())).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "a void nobody asks anything of must stay empty, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml"))
                .nodes("//demand"),
            Matchers.empty()
        );
    }
}
