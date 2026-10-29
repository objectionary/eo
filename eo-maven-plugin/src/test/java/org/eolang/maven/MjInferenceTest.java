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
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjInference}.
 *
 * @since 0.67.0
 */
@ExtendWith(MktmpResolver.class)
final class MjInferenceTest {

    @Test
    void writesTableOfWholeProgram(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "[] > app",
                "  [] > t",
                "    [] > next",
                ""
            )
        );
        maven.execute(MjParse.class).execute(MjInference.class);
        MatcherAssert.assertThat(
            "the facts must say what the innermost formation has, but they dont",
            new XMLDocument(
                maven.targetPath()
                    .resolve(maven.dirName("inference"))
                    .resolve("facts.xml")
            ),
            XhtmlMatchers.hasXPath("/facts/objects/type[@id='Φ.app.t']/attr[@name='next']")
        );
    }

    @Test
    void readsOnlyTheXmirOfARegisteredSource(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(System.lineSeparator(), "[] > app", "")
        );
        maven.execute(MjParse.class);
        Files.writeString(
            maven.targetPath().resolve(maven.dirName("parse")).resolve("bee.xmir"),
            new EoSyntax(
                String.join(System.lineSeparator(), "[] > bee", "")
            ).parsed().toString()
        );
        maven.execute(MjInference.class);
        MatcherAssert.assertThat(
            "an XMIR that no tojo names must stay out of the facts, but it didnt",
            new XMLDocument(
                maven.targetPath()
                    .resolve(maven.dirName("inference"))
                    .resolve("facts.xml")
            ).nodes("/facts/objects/type[@id='Φ.bee']"),
            Matchers.empty()
        );
    }

    @Test
    void writesFactsForModuleWithoutSources(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.execute(MjParse.class).execute(MjInference.class);
        MatcherAssert.assertThat(
            "the facts must be written for a module with no sources, but they werent",
            new XMLDocument(
                maven.targetPath()
                    .resolve(maven.dirName("inference"))
                    .resolve("facts.xml")
            ),
            XhtmlMatchers.hasXPath("/facts/calls[not(*)]")
        );
    }

    @Test
    void keepsPreparedXmirOfProgram(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "[x] > box",
                "  x.lid.hinge > @",
                ""
            )
        );
        maven.execute(MjParse.class).execute(MjInference.class);
        MatcherAssert.assertThat(
            "the prepared XMIR must have one object per name of a chain, but it doesnt",
            new XMLDocument(
                maven.targetPath()
                    .resolve(maven.dirName("pre-inference"))
                    .resolve("foo")
                    .resolve("x")
                    .resolve("main.xmir")
            ),
            XhtmlMatchers.hasXPath("//o[@base='.hinge']/o[@base='.lid']/o[@base='.x']/o[@base='ξ']")
        );
    }
}
