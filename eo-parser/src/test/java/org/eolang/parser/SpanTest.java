/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Span}.
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
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
    void exposesNullCharAsHeadOfBlank() {
        MatcherAssert.assertThat(
            "head of a blank line cannot point at any character",
            new Span("     ", 1).head(),
            Matchers.equalTo('\0')
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
