/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link LnMeta}.
 * @since 0.1
 */
final class LnMetaTest {

    @Test
    void emitsSimpleMeta() {
        final Emit emit = new Emit();
        new LnMeta(new Span("+foo", 1)).into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "a bare +foo must emit a meta with head='foo' and no parts",
            LnMetaTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta[@line='1']/head[text()='foo']",
                "/object/metas/meta[not(part)]"
            )
        );
    }

    @Test
    void emitsMetaWithSingleArgument() {
        final Emit emit = new Emit();
        new LnMeta(new Span("+alias org.example.foo", 2))
            .into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "a meta with one part must emit one <part> child carrying that text",
            LnMetaTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/metas/meta/part[text()='org.example.foo']"
            )
        );
    }

    @Test
    void emitsMetaWithMultipleParts() {
        final Emit emit = new Emit();
        new LnMeta(new Span("+rt jvm a.b.c:lib:1.0.0", 1))
            .into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "a multi-part meta must emit one <part> child per space-separated token",
            LnMetaTest.render(emit),
            XhtmlMatchers.hasXPath("/object/metas/meta[count(part)=2]")
        );
    }

    @Test
    void promotesRootQToPhiInPart() {
        final Emit emit = new Emit();
        new LnMeta(new Span("+rooted Q.org.eolang.x", 1))
            .into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "a part starting with Q. must be promoted to Φ. per R-3.2.3 / R-9.3",
            LnMetaTest.render(emit),
            XhtmlMatchers.hasXPath("/object/metas/meta/part[text()='Φ.org.eolang.x']")
        );
    }

    @Test
    void promotesBareQToPhi() {
        final Emit emit = new Emit();
        new LnMeta(new Span("+rooted Q", 1)).into(new Stack(), new Globals(), emit);
        MatcherAssert.assertThat(
            "a bare Q part must be promoted to Φ per R-3.2.3",
            LnMetaTest.render(emit),
            XhtmlMatchers.hasXPath("/object/metas/meta/part[text()='Φ']")
        );
    }

    @Test
    void rejectsMetaAtNonZeroIndent() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnMeta(new Span("  +foo", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a meta line at indent > 0 must be rejected per R-3.2.1"
        );
    }

    @Test
    void namesTheIndentItFoundInsteadOfBlamingOrdering() {
        MatcherAssert.assertThat(
            "an indented meta must name the indent it found, not the ordering rule",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("  +package foo", 1))
                    .into(new Stack(), new Globals(), new Emit()),
                "a meta line at indent 2 must be rejected per R-3.2.1"
            ).getMessage(),
            Matchers.equalTo("meta directive must sit at indent 0, found indent 2")
        );
    }

    @Test
    void namesADeeperIndentToo() {
        MatcherAssert.assertThat(
            "the message must carry whatever indent the meta actually sat at, not a fixed one",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("    +package foo", 1))
                    .into(new Stack(), new Globals(), new Emit()),
                "a meta line at indent 4 must be rejected per R-3.2.1"
            ).getMessage(),
            Matchers.equalTo("meta directive must sit at indent 0, found indent 4")
        );
    }

    @Test
    void reportsIndentAtAnOddDepthToo() {
        MatcherAssert.assertThat(
            "an odd, not just an even, indent must still be named in the message",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span(" +foo", 1))
                    .into(new Stack(), new Globals(), new Emit()),
                "a meta line at indent 1 must be rejected per R-3.2.1"
            ).getMessage(),
            Matchers.equalTo("meta directive must sit at indent 0, found indent 1")
        );
    }

    @Test
    void keepsPointingAtTheIndentedColumn() {
        MatcherAssert.assertThat(
            "the error position must still be the column of the offending indent",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("  +foo", 1))
                    .into(new Stack(), new Globals(), new Emit()),
                "a meta line at indent 2 must be rejected per R-3.2.1"
            ).pos(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void keepsReportingTheOffendingLineNumber() {
        MatcherAssert.assertThat(
            "the error must still name the source line the indented meta sat on",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("  +foo", 9))
                    .into(new Stack(), new Globals(), new Emit()),
                "a meta line at indent 2 on line 9 must be rejected per R-3.2.1"
            ).line(),
            Matchers.equalTo(9)
        );
    }

    @Test
    void rejectsOrderingSeparatelyFromIndent() {
        final Globals globals = new Globals();
        globals.markEmitted();
        MatcherAssert.assertThat(
            "a meta at indent 0 arriving after the first object keeps the ordering message",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("+foo", 5)).into(new Stack(), globals, new Emit()),
                "a meta arriving after the first non-meta object must be rejected per R-3.2.2"
            ).getMessage(),
            Matchers.equalTo("meta directive must precede all other objects")
        );
    }

    @Test
    void rejectsMetaAfterFirstObject() {
        final Globals globals = new Globals();
        globals.markEmitted();
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnMeta(new Span("+foo", 5)).into(new Stack(), globals, new Emit()),
            "a meta arriving after the first non-meta object must be rejected per R-3.2.2"
        );
    }

    @Test
    void reportsDoubleSpacePosition() {
        MatcherAssert.assertThat(
            "double space error must point at the second space",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("+foo a  b", 1))
                    .into(new Stack(), new Globals(), new Emit()),
                "double space between parts must be rejected per R-3.2.4"
            ).pos(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void rejectsATabBetweenParts() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnMeta(new Span("+rt jvm\torg.eolang:eo-runtime:0.0.0", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a tab between meta parts must be rejected, since only a single ASCII space separates them"
        );
    }

    @Test
    void reportsATabBetweenPartsWithTheCanonicalMessage() {
        MatcherAssert.assertThat(
            "a tab between meta parts must carry the §9.9 text of R-3.2.4, but it didnt",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnMeta(new Span("+rt jvm\torg.eolang:eo-runtime:0.0.0", 1))
                    .into(new Stack(), new Globals(), new Emit()),
                "a tab between meta parts must be rejected per R-3.2.4"
            ).getMessage(),
            Matchers.equalTo("meta parts must be separated by exactly one space")
        );
    }

    @Test
    void clearsPendingBlanksOnEmission() {
        final Globals globals = new Globals();
        globals.addComment(new Span("# doc", 1));
        globals.blank();
        new LnMeta(new Span("+foo", 3)).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a meta line is non-blank so it must reset pendingBlanks to zero",
            globals.pendingBlanks(),
            Matchers.equalTo(0)
        );
    }

    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
