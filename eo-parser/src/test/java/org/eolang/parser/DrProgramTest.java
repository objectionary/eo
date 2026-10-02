/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.StrictXML;
import com.jcabi.xml.XMLDocument;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.ResourceAccessMode;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link DrProgram}.
 *
 * @since 0.49
 */
final class DrProgramTest {

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ)
    void buildsProgramElement() throws Exception {
        MatcherAssert.assertThat(
            "XMIR program element is built",
            XhtmlMatchers.xhtml(
                new Xembler(new DrProgram()).xml()
            ),
            XhtmlMatchers.hasXPaths(
                "/object[@dob and @time and @version and @revision]"
            )
        );
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ)
    void setsSchemaLocation() throws Exception {
        MatcherAssert.assertThat(
            "XSD location is set",
            new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner())
                .element("object").attribute("xsi:noNamespaceSchemaLocation").text().get(),
            Matchers.not(Matchers.emptyString())
        );
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ)
    void checksThatSchemaLocationPointToFile() throws Exception {
        MatcherAssert.assertThat(
            "URL of XSD is set to file",
            new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner()).element("object")
                .attribute("xsi:noNamespaceSchemaLocation").text().get(),
            Matchers.startsWith("file:///")
        );
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ)
    void checksThatSchemaLocationPointToExistingFile() throws Exception {
        MatcherAssert.assertThat(
            "XSD file exists",
            Paths.get(
                new URI(
                    new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner())
                        .element("object").attribute("xsi:noNamespaceSchemaLocation").text().get()
                )
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ)
    void doesNotDuplicateSlashesInSchemaLocation() throws Exception {
        MatcherAssert.assertThat(
            "URL of XSD has no redundant slash after the scheme",
            new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner()).element("object")
                .attribute("xsi:noNamespaceSchemaLocation").text().get(),
            Matchers.not(Matchers.startsWith("file:////"))
        );
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ_WRITE)
    void honorsExplicitXmirXsdProperty() throws Exception {
        final Path file = Files.createTempFile("xmir", ".xsd");
        Files.write(file, "<xsd/>".getBytes(StandardCharsets.UTF_8));
        System.setProperty("xmir.xsd", file.toString());
        try {
            MatcherAssert.assertThat(
                "explicit xmir.xsd property overrides the relative guesses",
                new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner())
                    .element("object").attribute("xsi:noNamespaceSchemaLocation").text().get(),
                Matchers.equalTo(file.toUri().toString())
            );
        } finally {
            System.clearProperty("xmir.xsd");
            Files.delete(file);
        }
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ_WRITE)
    void ignoresMissingXmirXsdProperty() throws Exception {
        final Path missing = Paths.get("does-not-exist-1234567890.xsd").toAbsolutePath();
        System.setProperty("xmir.xsd", missing.toString());
        try {
            MatcherAssert.assertThat(
                "schema location falls back to a guess when the xmir.xsd property does not exist",
                new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner())
                    .element("object").attribute("xsi:noNamespaceSchemaLocation").text().get(),
                Matchers.not(Matchers.equalTo(missing.toUri().toString()))
            );
        } finally {
            System.clearProperty("xmir.xsd");
        }
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ_WRITE)
    void ignoresEmptyXmirXsdProperty() throws Exception {
        System.setProperty("xmir.xsd", "");
        try {
            MatcherAssert.assertThat(
                "schema location falls back to a guess when the xmir.xsd property is empty",
                new Xnav(new XMLDocument(new Xembler(new DrProgram()).xml()).inner())
                    .element("object").attribute("xsi:noNamespaceSchemaLocation").text().get(),
                Matchers.not(Matchers.emptyString())
            );
        } finally {
            System.clearProperty("xmir.xsd");
        }
    }

    @Test
    @ResourceLock(value = "xmir.xsd", mode = ResourceAccessMode.READ)
    void validatesAgainstSchema() {
        Assertions.assertDoesNotThrow(
            new StrictXML(
                new XMLDocument(
                    new Xembler(
                        new Directives().append(new DrProgram())
                            .add("listing").set("hello, world!").up()
                            .add("o").attr("name", "bar")
                    ).domQuietly()
                )
            )::inner,
            "XMIR document validates correctly"
        );
    }
}
