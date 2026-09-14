/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link Canonical}.
 *
 * @since 0.1
 */
final class CanonicalTest {

    @Test
    @Timeout(120L)
    void buildsALongTupleWithoutOverflowing() {
        final int size = 400;
        final StringBuilder xml = new StringBuilder(size * 24)
            .append("<object><o name='app'><o star='' line='1' pos='0'>");
        for (int idx = 0; idx < size; idx = idx + 1) {
            xml.append("<o base='Φ.e'/>");
        }
        MatcherAssert.assertThat(
            "a tuple literal must become one Φ.tuple layer per element, however many it holds, instead of overflowing Saxon as the layers are built",
            new Xsline(new StClasspath("/org/eolang/parser/parse/stars-to-tuples.xsl"))
                .pass(new XMLDocument(xml.append("</o></o></object>").toString()))
                .nodes("//o[@base='Φ.tuple']"),
            Matchers.hasSize(size)
        );
    }

    @Test
    void keepsAliasResolutionInDedicatedStage() {
        MatcherAssert.assertThat(
            "resolver must own every alias-bearing attribute",
            CanonicalTest.stylesheet(
                "org/eolang/parser/parse/resolve-aliases.xsl"
            ),
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='template' and contains(@match, '@base') and .//@*[contains(., \"head='alias'\")]]",
                "/*/*[local-name()='template' and contains(@match, '@atom') and .//@*[contains(., \"head='alias'\")]]",
                "/*/*[local-name()='template' and @match='@args' and .//@*[contains(., \"head='alias'\")]]",
                "/*/*[local-name()='template' and @match='@type' and .//@*[contains(., \"head='alias'\")]]"
            )
        );
    }

    @Test
    void keepsResolverBeforeDefaultPackage() {
        MatcherAssert.assertThat(
            "resolver must immediately precede default package homing",
            Canonical.XSLS.indexOf(
                "/org/eolang/parser/parse/resolve-aliases.xsl"
            ),
            Matchers.equalTo(
                Canonical.XSLS.indexOf(
                    "/org/eolang/parser/parse/add-default-package.xsl"
                ) - 1
            )
        );
    }

    @Test
    void keepsDefaultPackageFreeOfAliasRules() {
        MatcherAssert.assertThat(
            "default package homing must not resolve aliases",
            CanonicalTest.stylesheet(
                "org/eolang/parser/parse/add-default-package.xsl"
            ),
            Matchers.not(
                XhtmlMatchers.hasXPath(
                    "/*//@*[contains(., \"head='alias'\")]"
                )
            )
        );
    }

    private static XML stylesheet(final String path) {
        return new XMLDocument(
            new UncheckedText(
                new TextOf(new ResourceOf(path, CanonicalTest.class))
            ).asString()
        );
    }
}
