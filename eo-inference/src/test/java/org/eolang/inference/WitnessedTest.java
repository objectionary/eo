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
 * Test case for {@link Witnessed}.
 * @since 0.69.0
 */
@ExtendWith(MktmpResolver.class)
final class WitnessedTest {

    @Test
    void putsWhatWentIntoAVoidOnTheVoid(@Mktmp final Path temp) throws IOException {
        WitnessedTest.program(temp, "Φ.oak");
        new Witnessed(new Demanded(new Resolved(new Clues()))).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "the void must say what was put into it, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")).nodes(
                "/provides/type[@id='Φ.inc']/attr[@name='x']/witnessed/ref[@loc='Φ.oak']"
            ),
            Matchers.hasSize(1)
        );
    }

    @Test
    void saysNothingWhenTooManyThingsWentIn(@Mktmp final Path temp) throws IOException {
        WitnessedTest.program(temp, "Φ.oak", "Φ.elm");
        new Witnessed(new Demanded(new Resolved(new Clues())), 1).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "a choice too long to read must say so, but it was written out",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml"))
                .nodes("//witnessed/unknown"),
            Matchers.hasSize(1)
        );
    }

    @Test
    void keepsTheChoiceBetweenWhatWentIn(@Mktmp final Path temp) throws IOException {
        WitnessedTest.program(temp, "Φ.oak", "Φ.elm");
        new Witnessed(new Demanded(new Resolved(new Clues()))).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "two callers filling one void must make a choice of two, but they didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml"))
                .nodes("//witnessed/union/ref"),
            Matchers.hasSize(2)
        );
    }

    @Test
    void carriesWhatTheVoidHandedOnIsFilledWith(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("wood.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>",
                "<o base='∅' loc='Φ.inc.x' name='x'/></o>",
                "<o loc='Φ.oak' name='oak'/><o loc='Φ.elm' name='elm'/>",
                "<o base='Φ.inc' loc='Φ.app' name='app'>",
                "<o as='α0' base='Φ.oak' loc='Φ.app.α0'/></o>",
                "<o loc='Φ.bee' name='bee'>",
                "<o base='∅' loc='Φ.bee.y' name='y'/>",
                "<o as='φ' base='Φ.inc' loc='Φ.bee.φ'>",
                "<o as='α0' base='ξ.y' loc='Φ.bee.φ.α0'/></o></o>",
                "<o base='Φ.bee' loc='Φ.hut' name='hut'>",
                "<o as='α0' base='Φ.elm' loc='Φ.hut.α0'/></o></object>"
            )
        );
        new Witnessed(new Demanded(new Resolved(new Clues()))).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        MatcherAssert.assertThat(
            "what fills the void handed on must arrive in the choice, but it didnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")).nodes(
                "/provides/type[@id='Φ.inc']/attr[@name='x']/witnessed/union/ref[@loc='Φ.elm']"
            ),
            Matchers.hasSize(1)
        );
    }

    private static void program(final Path temp, final String... fillers) throws IOException {
        final StringBuilder text = new StringBuilder(
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>",
                "<o base='∅' loc='Φ.inc.x' name='x'/></o>",
                "<o loc='Φ.oak' name='oak'/><o loc='Φ.elm' name='elm'/>"
            )
        );
        for (int caller = 0; caller < fillers.length; caller += 1) {
            text.append(
                String.format(
                    "<o base='Φ.inc' loc='Φ.app%1$d' name='app%1$d'><o as='α0' base='%2$s' loc='Φ.app%1$d.α0'/></o>",
                    caller, fillers[caller]
                )
            );
        }
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("wood.xmir"),
            text.append("</object>").toString()
        );
    }
}
