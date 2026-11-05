/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
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
        final int size = 256;
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
    void acceptsTupleOfTheLargestSize() throws Exception {
        MatcherAssert.assertThat(
            "A tuple of 256 elements must parse without this error, but it didnt",
            new EoSyntax(CanonicalTest.tuple(256)).parsed().xpath(
                "/object/errors/error[contains(text(),'nested deeper')]/text()"
            ),
            Matchers.empty()
        );
    }

    @Test
    void refusesTupleOverTheLargestSize() throws Exception {
        MatcherAssert.assertThat(
            "A tuple of 1000 elements must be refused once, at its line, but it wasnt",
            new EoSyntax(CanonicalTest.tuple(1000)).parsed().xpath(
                "/object/errors/error[contains(text(),'nested deeper than 256 levels')]/@line"
            ),
            Matchers.contains("2")
        );
    }

    private static String tuple(final int size) {
        return String.format(
            "[] > app%n  * > @%n%s%n",
            IntStream.range(0, size)
                .mapToObj(idx -> String.format("    %d", idx))
                .collect(Collectors.joining(System.lineSeparator()))
        );
    }
}
