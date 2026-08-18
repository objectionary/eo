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
import org.junit.jupiter.params.provider.ValueSource;
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
            "characters that XML 1.0 text nodes can't hold must be dropped",
            ListingTest.listing(
                String.format(
                    "[] > x%c%c%c",
                    (char) 0x01,
                    (char) 0x07,
                    (char) 0x1F
                )
            ),
            Matchers.equalTo("[] > x")
        );
    }

    @Test
    void keepsCharactersLegalInXml10() {
        MatcherAssert.assertThat(
            "characters legal in an XML 1.0 text node must survive in the listing",
            ListingTest.listing(
                String.format(
                    "[] > x%c%c",
                    (char) 0x7F,
                    (char) 0x9F
                )
            ),
            Matchers.equalTo(
                String.format(
                    "[] > x%c%c",
                    (char) 0x7F,
                    (char) 0x9F
                )
            )
        );
    }

    @ParameterizedTest
    @ValueSource(ints = {0x09, 0x0A, 0x0D})
    void keepsControlCharactersLegalInXml(final int codepoint) {
        final String source = new String(Character.toChars(codepoint));
        MatcherAssert.assertThat(
            String.format(
                "Character '%s' (%s code) must survive between the forbidden C0 ranges",
                source, codepoint
            ),
            ListingTest.listing(source),
            Matchers.equalTo(source)
        );
    }

    @Test
    void doesNotThrowExceptionOnEmptySource() {
        Assertions.assertDoesNotThrow(
            () -> ListingTest.listing(""),
            "an empty source must not break the <listing> element"
        );
    }

    @Test
    void buildsListingForEmptySource() {
        MatcherAssert.assertThat(
            "An empty source must produce an empty listing",
            ListingTest.listing(""),
            Matchers.emptyString()
        );
    }

    @ParameterizedTest
    @ValueSource(
        ints = {
            0x00,
            0x01,
            0x08,
            0x0B,
            0x0C,
            0x0E,
            0x1F,
            0xFFFE,
            0xFFFF
        }
    )
    void removesForbiddenCharacters(final int codepoint) {
        final String source = new String(Character.toChars(codepoint));
        MatcherAssert.assertThat(
            String.format(
                "Character '%s' (%s code) must be removed from EO listing", source, codepoint
            ),
            ListingTest.listing(source),
            Matchers.emptyString()
        );
    }

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
