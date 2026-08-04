/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Listing}.
 * @since 0.1
 */
final class ListingTest {

    @ParameterizedTest
    @MethodSource("sources")
    void keepsSourceVerbatim(final String source) {
        MatcherAssert.assertThat(
            "the text of <listing> must be equal to the source, not escaped twice",
            ListingTest.listing(source),
            Matchers.equalTo(source)
        );
    }

    @Test
    void dropsCharactersForbiddenInXml() {
        MatcherAssert.assertThat(
            "characters that XML text nodes can't hold must be dropped",
            ListingTest.listing("[] > x\u0001\u0007\u001F\u007F"),
            Matchers.equalTo("[] > x")
        );
    }

    @Test
    void buildsListingForEmptySource() {
        Assertions.assertDoesNotThrow(
            () -> ListingTest.listing(""),
            "an empty source must not break the <listing> element"
        );
    }

    /**
     * Sources to embed into {@code <listing>}.
     * @return Stream of sources
     */
    private static Stream<Arguments> sources() {
        return Stream.of(
            "[] > foo",
            String.join(
                System.lineSeparator(),
                "[] > app",
                "  \"a < b & c > d\" > x",
                ""
            ),
            String.join(
                System.lineSeparator(),
                "# Comment with 'quotes' and \"double quotes\".",
                "[] > bar",
                ""
            ),
            String.join(
                System.lineSeparator(),
                "[] > x",
                "  Q.io.stdout \"守规矩\" > @",
                ""
            )
        ).map(Arguments::of);
    }

    /**
     * Read the text of {@code /object/listing} built for the given source.
     * @param source The EO source text
     * @return The text of the {@code listing} element
     */
    private static String listing(final String source) {
        return new Xnav(
            new XMLDocument(
                new Xembler(
                    new Directives().add("object").up().append(new Listing(source))
                ).xmlQuietly()
            ).inner()
        ).element("object").element("listing").text().orElse("");
    }
}
