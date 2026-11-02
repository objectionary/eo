/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.jucs.ClasspathSource;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Answers}.
 *
 * <p>Each pack in {@code packs} carries the EO sources of a small program
 * and the XPaths its {@code answers.xml} must match. The facts are made of
 * the sources the way Maven makes them, the engine walks them, and every
 * XPath must find a node in the answers it writes. A larger program is
 * checked byte for byte when its two files are named on the command line:
 * {@code -Dinference.facts=…/facts.xml -Dinference.answers=…/answers.xml},
 * the way eo-runtime is checked.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class AnswersTest {

    /**
     * Temp directory, injected into every test instance, since a
     * parameterized test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/inference/packs/", glob = "**.yaml")
    void writesAnswersOfPack(final String yaml) throws Exception {
        final Pack pack = new Pack(yaml);
        final Sources sources = pack.sources(this.dir);
        new Answers(new Walk(sources.facts(), sources.names()))
            .write(this.dir.resolve("answers.xml"));
        MatcherAssert.assertThat(
            String.format(
                "the answers written of the program must match every XPath of the pack, but they dont:%n%s",
                yaml
            ),
            new XMLDocument(this.dir.resolve("answers.xml")),
            XhtmlMatchers.hasXPaths(pack.answers())
        );
    }

    @Test
    @EnabledIfSystemProperty(named = "inference.facts", matches = ".+")
    void writesAnswersOfProgramNamedOnCommandLine(@Mktmp final Path temp) throws Exception {
        new Answers(
            new Walk(Paths.get(System.getProperty("inference.facts")), new Sources(temp).names())
        ).write(temp.resolve("answers.xml"));
        MatcherAssert.assertThat(
            "the answers written from the facts named on the command line must be the answers named there, byte for byte, but they werent",
            Files.readAllBytes(temp.resolve("answers.xml")),
            Matchers.equalTo(
                Files.readAllBytes(Paths.get(System.getProperty("inference.answers")))
            )
        );
    }
}
