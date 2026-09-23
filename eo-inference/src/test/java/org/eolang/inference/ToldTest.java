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
        ToldTest.program(
            temp,
            "<o base='∅' loc='Φ.inc.x' name='x'/>",
            ToldTest.caller("app", "Φ.oak")
        );
        MatcherAssert.assertThat(
            "a void the program fills one way must be typed as that, but it wasnt",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml")).nodes(
                "/provides/type[@id='Φ.inc']/attr[@name='x' and @settled='Φ.oak']"
            ),
            Matchers.hasSize(1)
        );
    }

    @Test
    void leavesAVoidFilledTwoWaysAlone(@Mktmp final Path temp) throws IOException {
        ToldTest.program(
            temp,
            "<o base='∅' loc='Φ.inc.x' name='x'/>",
            ToldTest.caller("app", "Φ.oak"),
            ToldTest.caller("hut", "Φ.elm")
        );
        MatcherAssert.assertThat(
            "a void filled two ways cannot be typed as either of them, but it was",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml"))
                .nodes("//attr[@settled]"),
            Matchers.empty()
        );
    }

    @Test
    void leavesAVoidTheSourceTypedAlone(@Mktmp final Path temp) throws IOException {
        ToldTest.program(
            temp,
            "<o base='∅' loc='Φ.inc.x' name='x' type='Φ.elm'/>",
            ToldTest.caller("app", "Φ.oak")
        );
        MatcherAssert.assertThat(
            "a sighting cannot be written next to what the source declared, but it was",
            new XMLDocument(temp.resolve("tables").resolve("provides.xml"))
                .nodes("//attr[@settled]"),
            Matchers.empty()
        );
    }

    private static String caller(final String name, final String filler) {
        return String.format(
            "<o base='Φ.inc' loc='Φ.%1$s' name='%1$s'><o as='α0' base='%2$s' loc='Φ.%1$s.α0'/></o>",
            name, filler
        );
    }

    private static void program(
        final Path temp, final String hollow, final String... callers
    ) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("wood.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>", hollow, "</o>",
                "<o loc='Φ.oak' name='oak'/><o loc='Φ.elm' name='elm'/>",
                String.join("", callers), "</object>"
            )
        );
        new Told(new Witnessed(new Demanded(new Resolved(new Clues())))).follow(
            temp.resolve("xmirs"), temp.resolve("tables")
        );
    }
}
