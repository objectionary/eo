/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.Canonical;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Facts}.
 *
 * <p>What the facts say about a program is checked by the packs in
 * {@code packs}: each one carries the EO sources of a small program, and a
 * pack whose rule is about the facts also carries the XPaths its
 * {@code facts.xml} must match. A source goes through the steps Maven takes:
 * the parser writes it with no indent and reads it back, {@link Canonical}
 * gets the dotted names of the files in subfolders, and {@link Prepared}
 * makes it ready. A pack with no XPaths of the facts is skipped here, and the
 * answers of every pack are checked by {@link AnswersTest}.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class FactsTest {

    /**
     * Temp directory, injected into every test instance, since a
     * parameterized test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/inference/packs/", glob = "**.yaml")
    void writesFactsOfPack(final String yaml) throws Exception {
        final Pack pack = new Pack(yaml);
        Assumptions.assumeTrue(
            pack.facts().length > 0, "the rule of the pack is not about the facts"
        );
        MatcherAssert.assertThat(
            String.format(
                "the facts written of the program must match every XPath of the pack, but they dont:%n%s",
                yaml
            ),
            new XMLDocument(pack.sources(this.dir).facts()),
            XhtmlMatchers.hasXPaths(pack.facts())
        );
    }
}
