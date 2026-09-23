/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjDealpha}.
 *
 * @since 0.69.0
 */
@ExtendWith(MktmpResolver.class)
final class MjDealphaTest {

    @Test
    void namesArgumentAfterVoidItLandsIn(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the argument must carry the name of the void it fills, but it doesnt",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[] > app",
                        "  [bar] > foo",
                        "    bar > @",
                        "  foo 42 > @"
                    )
                ).execute(new PpDealpha()).result().get("target/7-dealpha/foo/x/main.xmir")
            ),
            XhtmlMatchers.hasXPath("/object/o/o[@name='φ']/o[@as='bar']")
        );
    }

    @Test
    void namesSecondArgumentOfCurriedCopy(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the argument of a curried copy must fill the void left vacant, but it doesnt",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[] > app",
                        "  [left right] > pair",
                        "    left > @",
                        "  pair 1 > half",
                        "  half 2 > @"
                    )
                ).execute(new PpDealpha()).result().get("target/7-dealpha/foo/x/main.xmir")
            ),
            XhtmlMatchers.hasXPath("/object/o/o[@name='φ']/o[@as='right']")
        );
    }

    @Test
    void keepsAlphaInsideLiteral(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the bytes of a literal must stay positional, but they were renamed",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[as-bytes] > number",
                        "  as-bytes > @"
                    ),
                    "number",
                    "number.eo"
                ).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[] > app",
                        "  42 > @"
                    ),
                    "app",
                    "app.eo"
                ).execute(new PpDealpha()).result().get("target/7-dealpha/app.xmir")
            ),
            XhtmlMatchers.hasXPath("//o[@base='Φ.number']/o[@as='α0']")
        );
    }

    @Test
    void keepsAlphaOfBranches(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the branches of an 'if' must stay positional, but they were renamed",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[] > app",
                        "  [] > yes",
                        "    [left right] > if",
                        "      left > @",
                        "  yes.if 1 2 > @"
                    )
                ).execute(new PpDealpha()).result().get("target/7-dealpha/foo/x/main.xmir")
            ),
            XhtmlMatchers.hasXPath("/object/o/o[@name='φ']/o[@as='α1']")
        );
    }

    @Test
    void keepsAlphaOfApplicationOfVoid(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument of a void has nowhere to land, but it was renamed",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[f] > app",
                        "  f 7 > @"
                    )
                ).execute(new PpDealpha()).result().get("target/7-dealpha/foo/x/main.xmir")
            ),
            XhtmlMatchers.hasXPath("/object/o/o[@name='φ']/o[@as='α0']")
        );
    }

    @Test
    void failsOnAlphaLeftWhenAsked(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp).withProgram(
                String.join(
                    System.lineSeparator(),
                    "[f] > app",
                    "  f 7 > @"
                )
            ).with("rigid", true).execute(new PpDealpha()),
            "an argument left without a name must stop the build when asked to, but it didnt"
        );
    }

    @Test
    void pointsObjectAtDealphaXmir(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the object must be transpiled from the renamed XMIR, but it isnt",
            new FakeMaven(temp).withProgram(
                String.join(
                    System.lineSeparator(),
                    "[] > app",
                    "  [bar] > foo",
                    "    bar > @",
                    "  foo 42 > @"
                )
            ).execute(new PpDealpha()).foreignTojos().find("foo.x.main").xmir().toString(),
            Matchers.endsWith(Paths.get("7-dealpha/foo/x/main.xmir").toString())
        );
    }

    @Test
    void transpilesArgumentByName(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must bind the argument by its name, but it doesnt",
            new TextOf(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "+package examples",
                        "",
                        "[] > app",
                        "  [bar] > foo",
                        "    bar > @",
                        "  foo 42 > @"
                    )
                ).execute(new PpDealpha())
                    .execute(MjTranspile.class)
                    .result()
                    .get("target/generated/org/eolang/EO_examples/EOapp.java")
            ).asString(),
            Matchers.containsString("new Bind(\"bar\"")
        );
    }
}
