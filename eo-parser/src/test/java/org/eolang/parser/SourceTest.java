/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Source}.
 * @since 0.1
 */
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
        MatcherAssert.assertThat(
            "three Unix-terminated lines must yield three spans in order",
            SourceTest.texts(
                new Source(SourceTest.join(SourceTest.newline(), "alpha", "beta", "gamma"))
            ),
            Matchers.contains("alpha", "beta", "gamma")
        );
    }

    @Test
    void splitsOnWindowsLineEndings() {
        MatcherAssert.assertThat(
            "Windows CRLF must split lines the same way Unix LF does",
            SourceTest.texts(
                new Source(SourceTest.join(SourceTest.crnl(), "alpha", "beta", "gamma"))
            ),
            Matchers.contains("alpha", "beta", "gamma")
        );
    }

    @Test
    void keepsBareCarriageReturnInsideTheLine() {
        MatcherAssert.assertThat(
            "R-2.1.2 knows two line endings, and a lone CR is neither of them",
            SourceTest.texts(new Source("alpha".concat(SourceTest.creturn()).concat("beta"))),
            Matchers.contains("alpha".concat(SourceTest.creturn()).concat("beta"))
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
        MatcherAssert.assertThat(
            "a blank line between two non-blank lines must produce a blank span",
            SourceTest.texts(
                new Source(SourceTest.join(SourceTest.newline(), "alpha", "", "gamma"))
            ),
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
        MatcherAssert.assertThat(
            "a source mixing LF and CRLF must split correctly at each terminator",
            SourceTest.texts(
                new Source(
                    "alpha".concat(SourceTest.newline())
                        .concat("beta").concat(SourceTest.crnl())
                        .concat("gamma").concat(SourceTest.crnl())
                )
            ),
            Matchers.contains("alpha", "beta", "gamma")
        );
    }

    @Test
    void reusesTheSameSpansOnEveryIteration() {
        final Source source = new Source(
            SourceTest.join(SourceTest.newline(), "alpha", "beta", "gamma")
        );
        MatcherAssert.assertThat(
            "a second iteration must hand back the very same span objects, not fresh ones",
            SourceTest.collect(source),
            Matchers.contains(SourceTest.collect(source).toArray(new Span[0]))
        );
    }

    @Test
    void iteratesTwiceOverTheSameInstance() {
        final Source source = new Source(
            SourceTest.join(SourceTest.newline(), "alpha", "beta")
        );
        SourceTest.texts(source);
        MatcherAssert.assertThat(
            "iterating an already-iterated source must still yield all of its lines",
            SourceTest.texts(source),
            Matchers.contains("alpha", "beta")
        );
    }

    @Test
    void forbidsRemovalThroughTheIterator() {
        final Iterator<Span> iter = new Source(
            SourceTest.join(SourceTest.newline(), "alpha", "beta")
        ).iterator();
        iter.next();
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            iter::remove,
            "the iterator must not let a caller drop a span from the source"
        );
    }

    private static String join(final String sep, final String... rows) {
        return String.join(sep, rows);
    }

    private static String newline() {
        return String.valueOf((char) 10);
    }

    private static String creturn() {
        return String.valueOf((char) 13);
    }

    private static String crnl() {
        return SourceTest.creturn().concat(SourceTest.newline());
    }

    private static List<Span> collect(final Source source) {
        final List<Span> out = new ArrayList<>(0);
        for (final Span span : source) {
            out.add(span);
        }
        return out;
    }

    private static List<String> texts(final Source source) {
        final List<String> out = new ArrayList<>(0);
        for (final Span span : source) {
            out.add(span.text());
        }
        return out;
    }
}
