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
    void clearsPendingBlanksOnEmission() {
        final Globals globals = new Globals();
        globals.blank();
        new LnMeta(new Span("+foo", 2)).into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a meta line is non-blank so it must reset pendingBlanks to zero",
            globals.pendingBlanks(),
            Matchers.equalTo(0)
        );
    }

    /**
     * Render the emit's directives under a fresh {@code <object/>}.
     * @param emit The emit
     * @return XMIR document
     */
    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
