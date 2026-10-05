/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Listing}.
 *
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
    void keepsSourceVerbatimWithCrlf() {
        final String source = String.join(
            String.valueOf((char) 13).concat(String.valueOf((char) 10)),
            "[] > app",
            "  \"a < b & c > d\" > x",
            ""
        );
        MatcherAssert.assertThat(
            "the text of <listing> must preserve CRLF verbatim, regardless of platform",
            ListingTest.listing(source),
            Matchers.equalTo(source)
        );
    }

    @Test
    void omitsListingForEmptySource() {
        MatcherAssert.assertThat(
            "an empty source must produce no <listing>, since the schema forbids an empty one",
            ListingTest.xmir("").nodes("/object/listing"),
            Matchers.empty()
        );
    }

    @Test
    void omitsListingForFullyForbiddenSource() {
        MatcherAssert.assertThat(
            "a source made only of forbidden characters must leave no <listing> behind",
            ListingTest.xmir(String.format("%c%c", (char) 0x01, (char) 0x7F))
                .nodes("/object/listing"),
            Matchers.empty()
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
            0xFFFF,
            0x7F,
            0x80,
            0x84,
            0x86,
            0x9F
        }
    )
    void removesForbiddenCharacters(final int codepoint) {
        MatcherAssert.assertThat(
            String.format(
                "the character with code %s is not removed, or its neighbours went with it",
                codepoint
            ),
            ListingTest.listing(
                String.format("a%sb", new String(Character.toChars(codepoint)))
            ),
            Matchers.equalTo("ab")
        );
    }

    @Test
    void emptiesSourceOfForbiddenCharactersOnly() {
        MatcherAssert.assertThat(
            "a source holding nothing but forbidden characters leaves a listing that is not empty",
            ListingTest.listing(
                String.format("%c%c%c%c", (char) 0x00, (char) 0x0B, (char) 0x9F, (char) 0xFFFF)
            ),
            Matchers.emptyString()
        );
    }

    @Test
    void keepsSupplementaryCharacters() {
        final String source = String.format(
            "[] > x%s", new String(Character.toChars(0x1F600))
        );
        MatcherAssert.assertThat(
            "a character outside the Basic Multilingual Plane is not kept in the listing",
            ListingTest.listing(source),
            Matchers.equalTo(source)
        );
    }

    @ParameterizedTest
    @ValueSource(ints = {0x09, 0x0A, 0x0D, 0x20, 0x85, 0xA0, 0xFFFD})
    void keepsCharactersOutsideRestrictedRanges(final int codepoint) {
        final String source = String.format("x%cy", codepoint);
        MatcherAssert.assertThat(
            String.format(
                "Character with code %s falls between the restricted ranges and is not dropped",
                codepoint
            ),
            ListingTest.listing(source),
            Matchers.equalTo(source)
        );
    }

    @Test
    void leavesCursorOnObjectForTheNextSibling() {
        MatcherAssert.assertThat(
            "what the caller appends after <listing> must be its sibling under /object",
            new Xembler(
                new Directives()
                    .add("object").up()
                    .append(new Listing("[] > foo"))
                    .add("metas")
            ).xmlQuietly(),
            XhtmlMatchers.hasXPath("/object/metas")
        );
    }

    private static Stream<Arguments> sources() {
        final String eol = String.valueOf((char) 10);
        return Stream.of(
            "[] > foo",
            String.join(
                eol,
                "[] > app",
                "  \"a < b & c > d\" > x",
                ""
            ),
            String.join(
                eol,
                "# Comment with 'quotes' and \"double quotes\".",
                "[] > bar",
                ""
            ),
            String.join(
                eol,
                "[] > x",
                "  Q.io.stdout \"守规矩\" > @",
                ""
            )
        ).map(Arguments::of);
    }

    private static String listing(final String source) {
        return String.join("", ListingTest.xmir(source).xpath("/object/listing/text()"));
    }

    private static XML xmir(final String source) {
        return new XMLDocument(
            new Xembler(
                new Directives().add("object").up().append(new Listing(source))
            ).xmlQuietly()
        );
    }
}
