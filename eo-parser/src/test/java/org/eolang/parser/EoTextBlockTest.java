/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Eo} walking a text block.
 *
 * <p>Feeds EO source text spanning a triple-quoted block and asserts the
 * shape of the emitted XMIR, covering both the body consolidation and the
 * indent conditions the walker raises while the block stays open.</p>
 *
 * @since 0.1
 */
final class EoTextBlockTest {

    @Test
    void parsesTripleQuotedTextBlock() {
        MatcherAssert.assertThat(
            "a triple-quoted block must produce a Φ.string with bytes carrying the body",
            EoTextBlockTest.render(
                "[] > main",
                "  \"\"\"",
                "  hello",
                "  world",
                "  \"\"\" > greeting"
            ),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@name='greeting' and @base='Φ.string']"
            )
        );
    }

    @Test
    void stripsOpenerIndentFromTextBlockBody() {
        MatcherAssert.assertThat(
            "the body of a text block must have the opener indent stripped before joining",
            EoTextBlockTest.render(
                "[] > main",
                "  \"\"\"",
                "  hi",
                "  \"\"\" > x"
            ),
            XhtmlMatchers.hasXPath(
                "/object//o[@name='x']/o[@base='Φ.bytes']/o[text()='68-69']"
            )
        );
    }

    @Test
    void reportsTextBlockBodyLineWithNegativeIndent() {
        MatcherAssert.assertThat(
            "a body line indented less than the opener must surface an error per R-3.11.2",
            EoTextBlockTest.render(
                "[] > main",
                "  \"\"\"",
                "foo",
                "  \"\"\" > neg"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'text block body line indented less than opener')]"
            )
        );
    }

    @Test
    void acceptsTabIndentedTextBlockBodyMatchingOpenerIndent() {
        MatcherAssert.assertThat(
            "a body line whose leading whitespace counts as wide as the opener's must not surface indented-less-than-opener, whether that whitespace is spaces or tabs",
            EoTextBlockTest.render(
                "[] > main",
                "  \"\"\"",
                "\t\tfoo",
                "  \"\"\" > x"
            ),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void acceptsTabOnlyBlankLineInsideTextBlock() {
        MatcherAssert.assertThat(
            "a body line made entirely of tabs is blank and must not surface indented-less-than-opener",
            EoTextBlockTest.render(
                "[] > main",
                "  \"\"\"",
                "\t\t\t",
                "  \"\"\" > x"
            ),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void reportsUnclosedTextBlockAtEof() {
        MatcherAssert.assertThat(
            "a text block opened without a closer must surface unclosed-text-block at EOF",
            EoTextBlockTest.render(
                "[] > main",
                "  \"\"\"",
                "  unfinished"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'unclosed text block')]"
            )
        );
    }

    @Test
    void rollsBackAndRecoversFromInvalidTextBlockEscape() {
        MatcherAssert.assertThat(
            "an invalid text block escape must not corrupt later parsing",
            EoTextBlockTest.render(
                "[] > main", "  \"\"\"", "  bad \\q", "  \"\"\" > x", "[] > y"
            ),
            XhtmlMatchers.hasXPath("/object[errors/error[contains(text(),'escape')]][o[@name='y']]")
        );
    }

    private static String render(final String... rows) {
        final StringBuilder source = new StringBuilder(rows.length * 16);
        for (final String row : rows) {
            source.append(row).append((char) 10);
        }
        return new Xembler(
            new Directives().add("object").append(
                new Eo(source.toString()).directives()
            )
        ).xmlQuietly();
    }
}
