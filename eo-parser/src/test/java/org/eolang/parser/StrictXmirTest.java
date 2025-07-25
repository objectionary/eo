/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link StrictXmir}.
 *
 * @since 0.5
 * @todo #4360:30min Enable `StrictXmir` tests after XMIR.xsd release.
 *  Now tests fail because the latest schema available
 *  <a href="https://www.eolang.org/XMIR.xsd">here</a> does not allow `@author` attribute in the
 *  `object` element. Let's enable the tests right after we release new version.
 */
@Disabled
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
        MatcherAssert.assertThat(
            "temporary XSD file created",
            tmp.resolve("XMIR.xsd").toFile().exists(),
            Matchers.is(true)
        );
    }

    @RepeatedTest(20)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MktmpResolver.class)
    void doesNotFailWithDifferentXmlInMultipleThreads(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> {
                    final XML xml = StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd");
                    return new StrictXmir(xml, tmp).inner();
                }
            )::asList,
            "StrictXmir should not fail in different threads with different xmls"
        );
    }

    @RepeatedTest(20)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MktmpResolver.class)
    void doesNotFailOnTheSameOperation(@Mktmp final Path tmp) {
        final XML xmir = new StrictXmir(
            StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"), tmp
        );
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> xmir.inner()
            )::asList,
            "StrictXmir should not fail in different threads with the same xml"
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    @ExtendWith(WeAreOnline.class)
    void refersToAbsoluteFileName(@Mktmp final Path tmp) {
        MatcherAssert.assertThat(
            "XSD location must be absolute",
            Paths.get(
                new Xnav(
                    new StrictXmir(
                        StrictXmirTest.xmir("https://www.eolang.org/XMIR.xsd"), tmp
                    ).inner()
                ).element("object").attribute("xsi:noNamespaceSchemaLocation").text().get()
                    .substring("file:///".length())
            ).isAbsolute(),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchema(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new StrictXmir(
                new Xmir(
                    StrictXmirTest.xmir(
                        String.format(
                            "https://www.eolang.org/xsd/XMIR-%s.xsd",
                            Manifests.read("EO-Version")
                        )
                    )
                ),
                tmp
            )::inner,
            "validation should pass as normal"
        );
        MatcherAssert.assertThat(
            "temporary XSD file created",
            tmp.resolve(
                String.format("XMIR-%s.xsd", Manifests.read("EO-Version"))
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    @RepeatedTest(20)
    @ExtendWith(MktmpResolver.class)
    void validatesXmirWithLocalSchemaInMultipleThreads(@Mktmp final Path tmp) {
        Assertions.assertDoesNotThrow(
            new Together<>(
                thread -> new StrictXmir(
                    new Xmir(
                        StrictXmirTest.xmir(
                            String.format(
                                "https://www.eolang.org/xsd/XMIR-%s.xsd",
                                Manifests.read("EO-Version")
                            )
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
            new Xmir(
                StrictXmirTest.xmir(
                    String.format(
                        "https://www.eolang.org/xsd/XMIR-%s.xsd",
                        Manifests.read("EO-Version")
                    )
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
                new Xmir(
                    StrictXmirTest.xmir("https://www.invalid-website-uri/XMIR.xsd")
                ),
                tmp
            )::inner,
            "validation should fail because of broken URI"
        );
    }

    /**
     * Make a simple XMIR.
     * @param schema The schema
     */
    private static XML xmir(final String schema) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .append(new DrProgram())
                    .xpath("/object")
                    .attr("author", "noname")
                    .attr(
                        "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                        schema
                    )
                    .add("o")
                    .up()
            ).xmlQuietly()
        );
    }
}
