/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.manifests.Manifests;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.Together;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link StrictXmir}.
 *
 * @since 0.5
 */
final class StrictXmirTest {

    @Test
    @ExtendWith(MktmpResolver.class)
    @ExtendWith(WeAreOnline.class)
    void validatesXmir(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new StrictXmir(
                StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"),
                tmp
            )::inner,
            "validation should pass as normal"
        );
    }

    @RepeatedTest(20)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MktmpResolver.class)
    void doesNotFailWithDifferentXmlInMultipleThreads(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> {
                    final String schema;
                    if (thread % 2 == 0) {
                        schema = "https://www.eolang.org/XMIR.xsd";
                    } else {
                        schema = String.format(
                            "https://www.eolang.org/xsd/XMIR-%s.xsd",
                            StrictXmirTest.version()
                        );
                    }
                    return new StrictXmir(StrictXmirTest.xmir(schema), tmp).inner();
                }
            )::asList,
            "StrictXmir should not fail in different threads with different xmls"
        );
    }

    @RepeatedTest(20)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MktmpResolver.class)
    void doesNotFailOnTheSameOperation(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> new StrictXmir(
            StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"), tmp
        ).inner()
            )::asList,
            "StrictXmir should not fail in different threads with the same xml"
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    @ExtendWith(WeAreOnline.class)
    void refersToAbsoluteFileName(@Mktmp final Path tmp) throws Exception {
        MatcherAssert.assertThat(
            "XSD location must be absolute",
            Paths.get(
                new URI(
                    new Xnav(
                        new StrictXmir(
                            StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"), tmp
                        ).inner()
                    ).element("object").attribute("xsi:noNamespaceSchemaLocation").text().get()
                )
            ).isAbsolute(),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchemaWithoutException(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new StrictXmir(
                StrictXmirTest.xmir(
                    String.format(
                        "https://www.eolang.org/xsd/XMIR-%s.xsd",
                        StrictXmirTest.version()
                    )
                ),
                tmp
            )::inner,
            "validation should pass as normal"
        );
    }

    @ParameterizedTest
    @CsvSource({
        "'Φ.chunk.read.λ', true",
        "'Φ.set.+can-append-a-new-item.φ.ρ.ρ.α0', true",
        "'Φ.foo.bar', true",
        "'Φ', true",
        "'Φ.a.WRONG', false",
        "'Φ.a.λEXTRA', false"
    })
    void validatesLocatorAttribute(final String loc, final boolean valid) {
        final XML xml = new StrictXmir(StrictXmirTest.xmirWithLocator(loc));
        if (valid) {
            Assertions.assertDoesNotThrow(
                xml::inner,
                String.format("locator '%s' should have been accepted, but wasn't", loc)
            );
        } else {
            Assertions.assertThrows(
                IllegalArgumentException.class,
                xml::inner,
                String.format("locator '%s' should have been rejected, but wasn't", loc)
            );
        }
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchemaAndCreatesXsd(@Mktmp final Path tmp) {
        new StrictXmir(
            StrictXmirTest.xmir(
                String.format(
                    "https://www.eolang.org/xsd/XMIR-%s.xsd",
                    StrictXmirTest.version()
                )
            ),
            tmp
        ).inner();
        MatcherAssert.assertThat(
            "temporary XSD file created",
            tmp.resolve(
                String.format("XMIR-%s.xsd", StrictXmirTest.version())
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    void validatesXmirWithLocalSchemaAndEmptyCacheDirectory() throws IOException {
        try {
            Assertions.assertDoesNotThrow(
                new StrictXmir(
                    StrictXmirTest.xmir(
                        String.format(
                            "https://www.eolang.org/xsd/XMIR-%s.xsd",
                            StrictXmirTest.version()
                        )
                    ),
                    Path.of("")
                )::inner,
                "validation should not fail on a cache directory with no parent"
            );
        } finally {
            Files.deleteIfExists(
                Path.of(String.format("XMIR-%s.xsd", StrictXmirTest.version()))
            );
        }
    }

    @RepeatedTest(20)
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchemaInMultipleThreads(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> new StrictXmir(
                    StrictXmirTest.xmir(
                        String.format(
                            "https://www.eolang.org/xsd/XMIR-%s.xsd",
                            StrictXmirTest.version()
                        )
                    ),
                    tmp
                ).inner()
            )::asList,
            "validation should pass as normal"
        );
    }

    @RepeatedTest(20)
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchemaInMultipleThreadsWithTheSameXml(@Mktmp final Path tmp) {
        final XML xml = new StrictXmir(
            StrictXmirTest.xmir(
                String.format(
                    "https://www.eolang.org/xsd/XMIR-%s.xsd",
                    StrictXmirTest.version()
                )
            ),
            tmp
        );
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> xml.inner()
            )::asList,
            "validation should pass as normal"
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithBrokenUri(@Mktmp final Path tmp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            new StrictXmir(
                StrictXmirTest.xmir("https://www.invalid-website-uri/XMIR.xsd"),
                tmp
            )::inner,
            "validation should fail because of broken URI"
        );
    }

    private static String version() {
        return Manifests.read("EO-Version");
    }

    private static XML xmir(final String schema) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .append(new DrProgram())
                    .xpath("/object")
                    .attr("author", "noname").attr(
                        "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                        schema
                    )
                    .add("o")
                    .up()
            ).xmlQuietly()
        );
    }

    private static XML xmirWithLocator(final String loc) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .append(new DrProgram())
                    .xpath("/object")
                    .attr("author", "noname").attr(
                        "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                        String.format(
                            "https://www.eolang.org/xsd/XMIR-%s.xsd", StrictXmirTest.version()
                        )
                    )
                    .add("o").attr("loc", loc)
                    .up()
            ).xmlQuietly()
        );
    }
}
