/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.name;

import java.util.Optional;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link DelimitedName}.
 * @since 0.30
 */
final class DelimitedNameTest {
    @ParameterizedTest
    @MethodSource("titleLabelConcat")
    void retrievesTitle(final String expected, final Optional<String> label, final String concat) {
        final DelimitedName name = new DelimitedName(concat);
        MatcherAssert.assertThat(
            String.format("Can't retrieve title from %s", name),
            name.title(),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @MethodSource("titleLabelConcat")
    void retrievesLabel(final String title, final Optional<String> expected, final String concat) {
        final DelimitedName name = new DelimitedName(concat);
        MatcherAssert.assertThat(
            String.format("Can't retrieve label from %s", name),
            name.label(),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @MethodSource("titleLabelConcat")
    void convertsToString(final String title, final Optional<String> label, final String expected) {
        final DelimitedName name = new DelimitedName(title, label);
        MatcherAssert.assertThat(
            String.format("Can't convert title %s and label %s to string", title, label),
            name.toString(),
            Matchers.equalTo(expected)
        );
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Stream<Arguments> titleLabelConcat() {
        return Stream.of(
            Arguments.of("org.eolang.string", Optional.of("0.23.17"), "org.eolang.string|0.23.17"),
            Arguments.of("org.eolang.eagle", Optional.of(""), "org.eolang.eagle|"),
            Arguments.of("org.eolang.eagle", Optional.empty(), "org.eolang.eagle"),
            Arguments.of("org", Optional.of("eolang||eagle||"), "org|eolang||eagle||"),
            Arguments.of("", Optional.of("org|eolang||eagle"), "|org|eolang||eagle"),
            Arguments.of("", Optional.of("hello"), "|hello"),
            Arguments.of("", Optional.of(""), "|"),
            Arguments.of("", Optional.empty(), "")
        );
    }
}
