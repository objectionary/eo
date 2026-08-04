/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
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
import org.junit.jupiter.api.Test;
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

    @Test
    void doesNotWarnOnLocalWithoutName() {
        final List<String> warnings = new ArrayList<>(0);
        final Appender appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                if (event.getLevel().isGreaterOrEqual(Level.WARN)) {
                    warnings.add(String.valueOf(event.getRenderedMessage()));
                }
            }

            @Override
            public void close() {
                // Nothing to release: this appender holds no resources.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger root = Logger.getRootLogger();
        root.addAppender(appender);
        try {
            new Xsline(
                new StClasspath("/org/eolang/printer/print/restore-local-names.xsl")
            ).pass(
                new XMLDocument(
                    "<object><o base='.a'><o base='Q.d'><o base='x.r.b' local='h'/></o></o></object>"
                )
            );
        } finally {
            root.removeAppender(appender);
        }
        MatcherAssert.assertThat(
            "restore-local-names.xsl must not warn about an empty sequence when a @local node has no @name",
            warnings.stream().noneMatch(msg -> msg.contains("empty sequence")),
            Matchers.is(true)
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

    @Test
    void foldsAPayloadOnlyLiteralIntoAConst() {
        MatcherAssert.assertThat(
            "an argument whose own text is its whole payload, with no nested value, must fold into a const carrying that same text",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    "<p><o base='.as-bytes'><o base='Φ.dataized'><o base='Φ.bytes'>2A-</o></o></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[@base='Φ.bytes' and @const and text()='2A-']")
        );
    }

    @Test
    void foldsALiteralWhoseOwnPayloadIsNested() {
        MatcherAssert.assertThat(
            "an argument with no direct text of its own, only a nested value child (the everyday shape for a number literal), must fold into a const with that child untouched and no direct text of its own",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='.as-bytes'><o base='Φ.dataized'>",
                        "<o base='Φ.number'><o base='Φ.bytes'>2A-</o></o>",
                        "</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPath(
                "//o[@base='Φ.number' and @const and not(text()[normalize-space()]) and o[@base='Φ.bytes' and text()='2A-']]"
            )
        );
    }

    @Test
    void copiesTheOriginalUnchangedWhenNoArgumentExists() {
        MatcherAssert.assertThat(
            "a Φ.dataized wrapper with nothing to fold must be copied through untouched",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    "<p><o base='.as-bytes'><o base='Φ.dataized'/></o></p>"
                )
            ),
            XhtmlMatchers.hasXPath("//o[@base='.as-bytes']/o[@base='Φ.dataized' and not(node())]")
        );
    }

    @Test
    void keepsLiteralPayloadApartFromArgument() {
        MatcherAssert.assertThat(
            "the payload of a data literal must not be glued to the text of its argument - this fragment is the shape #5721 would produce, not one the current pipeline can build yet (#6102)",
            new Xsline(new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl")).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<p><o base='.as-bytes' name='x'><o base='Φ.dataized'>",
                        "<o base='Φ.bytes'>01-02<o base='Φ.number'>5</o></o>",
                        "</o></o></p>"
                    )
                )
            ),
            XhtmlMatchers.hasXPath(
                "//o[@base='Φ.bytes' and @const and text()='01-02' and o[text()='5']]"
            )
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
