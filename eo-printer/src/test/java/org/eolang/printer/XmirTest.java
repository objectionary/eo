/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Together;
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
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
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
import org.junit.jupiter.api.RepeatedTest;
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
            Matchers.equalTo(this.printed(xtory))
        );
    }

    @Test
    void doesNotLeakHelperNamespaces() {
        MatcherAssert.assertThat(
            "XSL helper namespaces must not be serialized into printer XML",
            new Xsline(
                new StClasspath("/org/eolang/printer/print/to-eo-tree.xsl")
            ).pass(
                new XMLDocument(
                    "<object><metas/><o name='main'/></object>"
                )
            ).toString(),
            Matchers.allOf(
                Matchers.not(Matchers.containsString("xmlns:eo=")),
                Matchers.not(Matchers.containsString("xmlns:xs="))
            )
        );
    }

    @Test
    void keepsArgumentsOfAnIdentityShapedFormation() {
        MatcherAssert.assertThat(
            "a formation that decorates its own void but also carries arguments cannot fold into the I glyph, which leaves nowhere for those arguments to go",
            new Xsline(
                new StClasspath("/org/eolang/printer/print/to-eo-tree.xsl")
            ).pass(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><metas/><o name='y'>",
                        "<o base='∅' name='m'/><o base='ξ.m' name='φ'/>",
                        "<o base='Φ.number'>5</o>",
                        "</o></object>"
                    )
                )
            ),
            XhtmlMatchers.hasXPath("//line[@base='[m]']")
        );
    }

    @RepeatedTest(3)
    void printsSameEoInManyThreads() {
        final XML xml = new XMLDocument(
            String.join(
                "",
                "<object><metas/><o name='foo'>",
                "<o name='a🌵1'><o base='Φ.number' name='@'>42</o></o>",
                "<o base='ξ.a🌵1' name='bar'/>",
                "</o></object>"
            )
        );
        MatcherAssert.assertThat(
            "Printing in parallel threads cannot diverge from printing in one",
            new Together<>(thread -> new Xmir(xml).toEO()),
            Matchers.everyItem(Matchers.equalTo(new Xmir(xml).toEO()))
        );
    }

    @Test
    void avoidsRepeatingHostedLookup() {
        MatcherAssert.assertThat(
            "The hosted template must not repeat the full first-host lookup",
            this.mergeMonikers(),
            XhtmlMatchers.hasXPaths(
                "/*[local-name()='stylesheet' and @version='2.0']",
                "/*/*[local-name()='function' and @name='eo:moniker-refs' and not(@cache)]",
                "/*/*[local-name()='function' and @name='eo:hosted-binding' and not(@cache)]",
                "/*/*[local-name()='template' and @priority='1']/*[local-name()='variable' and @name='owner' and @select='ancestor::o[eo:abstract(.)][1]']",
                "/*/*[local-name()='template' and @priority='1']/*[local-name()='variable' and @name='binding' and @select=\"key('moniker-binding', concat(generate-id($owner), ' ', eo:resolved-ref(.)), root(.))[1]\"]"
            )
        );
    }

    @Test
    void guardsExpensiveTemplatePredicates() {
        MatcherAssert.assertThat(
            "Cheap predicates must reject nodes before hosted/applied lookups",
            this.mergeMonikers(),
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='template' and @match=\"o[starts-with(@base, $eo:xi-dot)][exists(eo:hosted-binding(.))]\"]",
                "/*/*[local-name()='template' and @match=\"o[starts-with(@base, $eo:xi-dot)][exists(o)][not(exists(@name))][exists(eo:applied-handle(.))]\"]"
            )
        );
    }

    @Test
    void sortsOnlyMultipleDispatches() {
        MatcherAssert.assertThat(
            "Dispatch ordering must sort only when at least two candidates exist",
            this.mergeMonikers(),
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='function' and @name='eo:moniker-refs']/*[local-name()='variable' and @name='dispatch']/*[local-name()='choose']/*[local-name()='when' and @test='exists($dispatches[2])']/*[local-name()='perform-sort' and @select='$dispatches']",
                "/*/*[local-name()='function' and @name='eo:moniker-refs']/*[local-name()='variable' and @name='dispatch']/*[local-name()='choose']/*[local-name()='otherwise']/*[local-name()='sequence' and @select='$dispatches']"
            )
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
        final String printed = this.printed(xtory);
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

    private String printed(final Xtory xtory) {
        final String origin = (String) xtory.map().get("origin");
        final String expected;
        if (xtory.map().containsKey("printed")) {
            expected = (String) xtory.map().get("printed");
            MatcherAssert.assertThat(
                "The 'printed' section repeats 'origin' verbatim and must be deleted from the pack, since a pack without 'printed' already expects the printer to reproduce its 'origin'",
                expected,
                Matchers.not(Matchers.equalTo(origin))
            );
        } else {
            expected = origin;
        }
        return expected;
    }

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

    private XML mergeMonikers() {
        return new XMLDocument(
            new UncheckedText(
                new TextOf(
                    new ResourceOf(
                        "org/eolang/printer/print/merge-monikers.xsl",
                        XmirTest.class
                    )
                )
            ).asString()
        );
    }

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
