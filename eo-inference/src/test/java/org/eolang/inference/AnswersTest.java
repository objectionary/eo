/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Answers}.
 *
 * <p>Each pack in {@code packs} carries the {@code facts.xml} of a small
 * program and the {@code answers.xml} the Python engine wrote from it, and
 * the engine here must write the same bytes from the same facts. A larger
 * program is checked the same way when its two files are named on the
 * command line: {@code -Dinference.facts=…/facts.xml
 * -Dinference.answers=…/answers.xml}, the way eo-runtime is checked.</p>
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
        final Map<String, Object> pack = new XtSticky(new XtYaml(yaml)).map();
        final Path facts = this.dir.resolve("facts.xml");
        Files.write(facts, pack.get("facts").toString().getBytes(StandardCharsets.UTF_8));
        new Answers(new Walk(facts, new Sources(this.dir).names()))
            .write(this.dir.resolve("answers.xml"));
        MatcherAssert.assertThat(
            "the answers written from the facts of the pack must be the answers of the pack, byte for byte, but they werent",
            new String(
                Files.readAllBytes(this.dir.resolve("answers.xml")), StandardCharsets.UTF_8
            ),
            Matchers.equalTo(pack.get("answers").toString())
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
