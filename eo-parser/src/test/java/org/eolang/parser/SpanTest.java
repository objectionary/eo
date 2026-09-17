/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Span}.
 *
 * @since 0.1
 */
final class SpanTest {

    @Test
    void reportsIndentForLeadingSpaces() {
        MatcherAssert.assertThat(
            "indent must equal the count of leading space characters",
            new Span("    body", 1).indent(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void reportsZeroIndentWhenNoLeadingSpaces() {
        MatcherAssert.assertThat(
            "a line that starts in column 0 must have indent 0",
            new Span("name", 7).indent(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void detectsTrailingSpace() {
        MatcherAssert.assertThat(
            "a line ending in a space must report trailing whitespace",
            new Span("[] > foo ", 1).trailing(),
            Matchers.is(true)
        );
    }

    @Test
    void detectsTrailingTab() {
        MatcherAssert.assertThat(
            "a line ending in a tab must report trailing whitespace",
            new Span("[] > foo\t", 1).trailing(),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresFormFeedAsTrailingWhitespace() {
        MatcherAssert.assertThat(
            "a line ending in a form feed is not trailing whitespace under R-2.2.5",
            new Span("[] > foo\f", 1).trailing(),
            Matchers.is(false)
        );
    }

    @Test
    void ignoresVerticalTabAsTrailingWhitespace() {
        MatcherAssert.assertThat(
            "a line ending in a vertical tab is not trailing whitespace under R-2.2.5",
            new Span("[] > foo", 1).trailing(),
            Matchers.is(false)
        );
    }

    @Test
    void ignoresNonBreakingSpaceAsTrailingWhitespace() {
        MatcherAssert.assertThat(
            "a line ending in a non-breaking space is not trailing whitespace under R-2.2.5",
            new Span("[] > foo ", 1).trailing(),
            Matchers.is(false)
        );
    }

    @Test
    void rejectsTrailingWhitespaceOnBlankLine() {
        MatcherAssert.assertThat(
            "a blank line must not report trailing whitespace",
            new Span("    ", 1).trailing(),
            Matchers.is(false)
        );
    }

    @Test
    void detectsBlankLine() {
        MatcherAssert.assertThat(
            "a line of pure spaces must report blank",
            new Span("    ", 3).blank(),
            Matchers.is(true)
        );
    }

    @Test
    void detectsEmptyLineAsBlank() {
        MatcherAssert.assertThat(
            "an empty line must report blank",
            new Span("", 9).blank(),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsBlankWhenContentPresent() {
        MatcherAssert.assertThat(
            "a line with any non-space content cannot be blank",
            new Span("  x", 1).blank(),
            Matchers.is(false)
        );
    }

    @Test
    void exposesBodyAfterIndent() {
        MatcherAssert.assertThat(
            "body must be the line slice after the leading whitespace",
            new Span("    foo bar", 2).body(),
            Matchers.equalTo("foo bar")
        );
    }

    @Test
    void exposesEmptyBodyForBlank() {
        MatcherAssert.assertThat(
            "body of a blank line cannot contain anything",
            new Span("   ", 1).body(),
            Matchers.equalTo("")
        );
    }

    @Test
    void exposesFirstNonSpaceAsHead() {
        MatcherAssert.assertThat(
            "head must be the first non-space character of the line",
            new Span("    [args] > foo", 1).head(),
            Matchers.equalTo('[')
        );
    }

    @Test
    void rejectsHeadOfBlank() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Span("     ", 1).head(),
            "head of a blank line has no first non-whitespace character to return"
        );
    }

    @Test
    void rejectsHeadOfEmptyLine() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Span("", 1).head(),
            "head of an empty line has no first non-whitespace character to return"
        );
    }

    @Test
    void rejectsHeadOfTabOnlyLine() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Span("\t", 4).head(),
            "head of a tab-only line has no first non-whitespace character to return"
        );
    }

    @Test
    void detectsTabOnlyLineAsBlank() {
        MatcherAssert.assertThat(
            "a line made of a single tab is entirely whitespace and must report blank",
            new Span("\t", 4).blank(),
            Matchers.is(true)
        );
    }

    @Test
    void countsTabsInIndent() {
        MatcherAssert.assertThat(
            "every leading whitespace character counts towards the indent, tabs included",
            new Span("\t\t", 4).indent(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void exposesEmptyBodyForTabOnlyLine() {
        MatcherAssert.assertThat(
            "body of a tab-only line cannot contain anything",
            new Span(" \t ", 1).body(),
            Matchers.equalTo("")
        );
    }

    @Test
    void detectsTabInLeadingWhitespace() {
        MatcherAssert.assertThat(
            "a tab inside the leading-whitespace region must be reported",
            new Span(" \t  x", 1).tab(),
            Matchers.is(true)
        );
    }

    @Test
    void detectsTabAfterNonSpaceNonTabWhitespace() {
        MatcherAssert.assertThat(
            "a tab past a form-feed that indent() already counted must still be reported",
            new Span("\f\tfoo", 1).tab(),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresNonTabWhitespaceWithNoTabPresent() {
        MatcherAssert.assertThat(
            "leading whitespace with no tab at all must not be reported as tabbed",
            new Span("\ffoo", 1).tab(),
            Matchers.is(false)
        );
    }

    @Test
    void ignoresTabAfterFirstNonSpace() {
        MatcherAssert.assertThat(
            "a tab past the first non-space character is irrelevant for the indent error",
            new Span("  foo\tbar", 1).tab(),
            Matchers.is(false)
        );
    }

    @Test
    void retainsOriginalLineText() {
        MatcherAssert.assertThat(
            "text must round-trip the constructor body verbatim",
            new Span("  hello", 5).text(),
            Matchers.equalTo("  hello")
        );
    }

    @Test
    void retainsOneIndexedLineNumber() {
        MatcherAssert.assertThat(
            "line number must be preserved as supplied",
            new Span("anything", 42).line(),
            Matchers.equalTo(42)
        );
    }
}
