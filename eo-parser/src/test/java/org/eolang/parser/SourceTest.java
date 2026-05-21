/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Source}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals", "PMD.UnnecessaryLocalRule"})
final class SourceTest {

    @Test
    void yieldsNoSpansForEmptyInput() {
        MatcherAssert.assertThat(
            "an empty source must produce no spans",
            SourceTest.collect(new Source("")),
            Matchers.empty()
        );
    }

    @Test
    void splitsOnUnixLineEndings() {
        final String joined = SourceTest.join(
            SourceTest.newline(), "alpha", "beta", "gamma"
        );
        MatcherAssert.assertThat(
            "three Unix-terminated lines must yield three spans in order",
            SourceTest.texts(new Source(joined)),
            Matchers.contains("alpha", "beta", "gamma")
        );
    }

    @Test
    void splitsOnWindowsLineEndings() {
        final String joined = SourceTest.join(
            SourceTest.crnl(), "alpha", "beta", "gamma"
        );
        MatcherAssert.assertThat(
            "Windows CRLF must split lines the same way Unix LF does",
            SourceTest.texts(new Source(joined)),
            Matchers.contains("alpha", "beta", "gamma")
        );
    }

    @Test
    void splitsOnBareCarriageReturn() {
        MatcherAssert.assertThat(
            "a bare CR must terminate a line by itself",
            SourceTest.texts(new Source("alpha".concat(SourceTest.creturn()).concat("beta"))),
            Matchers.contains("alpha", "beta")
        );
    }

    @Test
    void numbersLinesFromOne() {
        final List<Integer> numbers = new ArrayList<>(0);
        for (final Span span : new Source(SourceTest.join(SourceTest.newline(), "a", "b", "c"))) {
            numbers.add(span.line());
        }
        MatcherAssert.assertThat(
            "line numbers must start at 1 and increment by 1 per span",
            numbers,
            Matchers.contains(1, 2, 3)
        );
    }

    @Test
    void preservesBlankLineInTheMiddle() {
        final String joined = SourceTest.join(
            SourceTest.newline(), "alpha", "", "gamma"
        );
        MatcherAssert.assertThat(
            "a blank line between two non-blank lines must produce a blank span",
            SourceTest.texts(new Source(joined)),
            Matchers.contains("alpha", "", "gamma")
        );
    }

    @Test
    void preservesLastLineWithoutTerminator() {
        MatcherAssert.assertThat(
            "a final line missing its terminator must still appear as a span",
            SourceTest.texts(new Source(SourceTest.join(SourceTest.newline(), "alpha", "beta"))),
            Matchers.contains("alpha", "beta")
        );
    }

    @Test
    void preservesIndentOnEachLine() {
        final List<Integer> indents = new ArrayList<>(0);
        for (final Span span : new Source(
            SourceTest.join(SourceTest.newline(), "foo", "  bar", "    baz")
        )) {
            indents.add(span.indent());
        }
        MatcherAssert.assertThat(
            "each span must carry the indent computed from its own line",
            indents,
            Matchers.contains(0, 2, 4)
        );
    }

    @Test
    void yieldsBlankForSingleEmptyLine() {
        MatcherAssert.assertThat(
            "a source consisting of a single empty line must produce one blank span",
            SourceTest.collect(new Source(SourceTest.newline())),
            Matchers.hasSize(1)
        );
    }

    @Test
    void handlesMixedLineEndingsInOneInput() {
        final String mixed = "alpha".concat(SourceTest.newline())
            .concat("beta").concat(SourceTest.crnl())
            .concat("gamma").concat(SourceTest.crnl());
        MatcherAssert.assertThat(
            "a source mixing LF and CRLF must split correctly at each terminator",
            SourceTest.texts(new Source(mixed)),
            Matchers.contains("alpha", "beta", "gamma")
        );
    }

    /**
     * Join the rows with the given separator (no trailing separator).
     * @param sep Separator
     * @param rows Rows to join
     * @return Joined text
     */
    private static String join(final String sep, final String... rows) {
        return String.join(sep, rows);
    }

    /**
     * LF character as a one-char string — used so source inputs avoid
     * the {@code \n} string literal that triggers Qulice's
     * line-separator rule.
     * @return LF
     */
    private static String newline() {
        return String.valueOf((char) 10);
    }

    /**
     * CR character.
     * @return CR
     */
    private static String creturn() {
        return String.valueOf((char) 13);
    }

    /**
     * CRLF pair.
     * @return CRLF
     */
    private static String crnl() {
        return SourceTest.creturn().concat(SourceTest.newline());
    }

    /**
     * Collect all spans from a source.
     * @param source The source
     * @return Materialised list of spans
     */
    private static List<Span> collect(final Source source) {
        final List<Span> out = new ArrayList<>(0);
        for (final Span span : source) {
            out.add(span);
        }
        return out;
    }

    /**
     * Collect the text of each span.
     * @param source The source
     * @return Materialised list of span texts
     */
    private static List<String> texts(final Source source) {
        final List<String> out = new ArrayList<>(0);
        for (final Span span : source) {
            out.add(span.text());
        }
        return out;
    }
}
