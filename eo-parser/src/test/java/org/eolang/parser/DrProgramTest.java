/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.StrictXML;
import com.jcabi.xml.XMLDocument;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link DrProgram}.
 *
 * @since 0.49
 */
final class DrProgramTest {

    @Test
    void buildsProgramElement() throws Exception {
        MatcherAssert.assertThat(
            "XMIR program element is built",
            XhtmlMatchers.xhtml(
                new Xembler(new DrProgram("foo")).xml()
            ),
            XhtmlMatchers.hasXPaths(
                "/program[@name='foo']",
                "/program[@dob and @time and @version and @revision]"
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void setsSchemaLocation() throws Exception {
        MatcherAssert.assertThat(
            "XSD location is set",
            new XMLDocument(new Xembler(new DrProgram("xxx")).xml()).toString(),
            Matchers.containsString(
                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void checksThatSchemaLocationPointToFile() throws Exception {
        MatcherAssert.assertThat(
            "URL of XSD is set to file",
            new XMLDocument(new Xembler(new DrProgram("bar")).xml()).xpath(
                "/program/@xsi:noNamespaceSchemaLocation"
            ).get(0),
            Matchers.startsWith("file:///")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void checksThatSchemaLocationPointToExistingFile() throws Exception {
        MatcherAssert.assertThat(
            "XSD file exists",
            Paths.get(
                new XMLDocument(new Xembler(new DrProgram("boom")).xml()).xpath(
                    "/program/@xsi:noNamespaceSchemaLocation"
                ).get(0).substring("file:///".length())
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    void validatesAgainstSchema() {
        Assertions.assertDoesNotThrow(
            new StrictXML(
                new XMLDocument(
                    new Xembler(
                        new Directives().append(new DrProgram("foo"))
                            .add("listing").set("hello, world!").up()
                            .add("objects").add("o").attr("name", "bar")
                    ).domQuietly()
                )
            )::inner,
            "XMIR document validates correctly"
        );
    }
}
