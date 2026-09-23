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
 * Test case for {@link Told}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class ToldTest {

    @Test
    void writesWhatTheOneCallerSettlesTheVoidAt(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void the program fills one way must be typed as that, but it wasnt",
            ToldTest.program(temp, "<o base='∅' loc='Φ.inc.x' name='x'/>", "Φ.oak").nodes(
                "/provides/type[@id='Φ.inc']/attr[@name='x' and @settled='Φ.oak']"
            ),
            Matchers.hasSize(1)
        );
    }

    @Test
    void leavesAVoidFilledTwoWaysAlone(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void filled two ways cannot be typed as either of them, but it was",
            ToldTest.program(
                temp, "<o base='∅' loc='Φ.inc.x' name='x'/>", "Φ.oak", "Φ.elm"
            ).nodes("//attr[@settled]"),
            Matchers.empty()
        );
    }

    @Test
    void leavesAVoidTheSourceTypedAlone(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a sighting cannot be written next to what the source declared, but it was",
            ToldTest.program(
                temp, "<o base='∅' loc='Φ.inc.x' name='x' type='Φ.elm'/>", "Φ.oak"
            ).nodes("//attr[@settled]"),
            Matchers.empty()
        );
    }

    private static XMLDocument program(
        final Path temp, final String hollow, final String... fillers
    ) throws IOException {
        final StringBuilder text = new StringBuilder(
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>", hollow,
                "</o><o loc='Φ.oak' name='oak'/><o loc='Φ.elm' name='elm'/>"
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
        new Told(new Witnessed(new Demanded(new Resolved(new Clues())))).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
        return new XMLDocument(temp.resolve("tables").resolve("provides.xml"));
    }
}
