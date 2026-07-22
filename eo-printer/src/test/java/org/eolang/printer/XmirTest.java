/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.yegor256.xsline.TrDefault;
import java.io.IOException;
import java.util.EnumMap;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.parser.TrFull;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtStrictAfter;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Xmir}.
 * @since 0.5
 */
final class XmirTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/printer/print-packs/yaml", glob = "**.yaml")
    void printsToEo(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final Xmir xmir = this.asXmir(
            (String) xtory.map().get("origin"), this.weights(xtory)
        );
        MatcherAssert.assertThat(
            String.format(
                "Result EO should be equal to original EO, XMIR is:%n%s",
                xmir
            ),
            xmir.toEO(),
            Matchers.equalTo(xtory.map().get("printed"))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/printer/print-packs/yaml", glob = "**.yaml")
    void printsToParseableEo(final String pack) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        Assumptions.assumeTrue(
            !Boolean.FALSE.equals(xtory.map().get("reprints")),
            "'reprints: false' packs need not reprint to themselves (#5739)"
        );
        final String printed = (String) xtory.map().get("printed");
        MatcherAssert.assertThat(
            String.format(
                "Expected EO should reprint to itself, but was:%n%s",
                printed
            ),
            this.asXmir(printed, this.weights(xtory)).toEO(),
            Matchers.equalTo(printed)
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/printer/eo-packs/print/", glob = "**.yaml")
    void checksPrintPacks(final String yaml) {
        final Xtory story = new XtSticky(
            new XtStrictAfter(
                new XtYaml(
                    yaml,
                    eo -> new EoSyntax(
                        String.format("%s%n", eo), new TrDefault<>()
                    ).parsed(),
                    new TrFull()
                )
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        MatcherAssert.assertThat(
            "The printing XSL sheet should transform XMIR as expected",
            story,
            new XtoryMatcher()
        );
    }

    /**
     * Convert EO to XMIR.
     * @param program Program in EOLANG
     * @param config The penalty weights to print with
     * @return XMIR
     */
    private Xmir asXmir(final String program, final Map<PenaltyKey, Integer> config)
        throws IOException {
        final XML xml = new EoSyntax(new InputOf(program)).parsed();
        MatcherAssert.assertThat(
            "Original EO should be parsed without errors",
            xml,
            Matchers.not(XhtmlMatchers.hasXPath("//errors/error"))
        );
        return new Xmir(xml, config);
    }

    /**
     * Read the penalty weights from a story's {@code penalties} block.
     *
     * <p>Every print-pack pins the full set of {@link PenaltyKey} weights, so
     * the expected layout is deterministic and does not depend on the defaults
     * baked into the printer. The block is a plain mapping of key name to
     * integer.</p>
     *
     * @param xtory The story
     * @return The weights, by key
     */
    private Map<PenaltyKey, Integer> weights(final Xtory xtory) {
        final Object block = xtory.map().get("penalties");
        MatcherAssert.assertThat(
            "Each print-pack must declare a 'penalties' block",
            block,
            Matchers.notNullValue()
        );
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        for (final Map.Entry<?, ?> entry : ((Map<?, ?>) block).entrySet()) {
            weights.put(
                PenaltyKey.valueOf((String) entry.getKey()),
                ((Number) entry.getValue()).intValue()
            );
        }
        return weights;
    }
}
