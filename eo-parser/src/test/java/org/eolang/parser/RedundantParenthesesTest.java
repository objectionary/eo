/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package org.eolang.parser;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.stream.Stream;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test cases for checking of redundant parentheses.
 * The {@link org.eolang.parser.Syntax} object is used in order to check all possible expressions
 * and situations.
 *
 * @since 0.28.12
 */
class RedundantParenthesesTest {

    @ParameterizedTest
    @MethodSource("testCases")
    void checksIfBracketsIsNotRedundant(
        final String program,
        final boolean correct
    ) throws IOException {
        final Syntax syntax = new Syntax(
            "foo",
            new InputOf(program),
            new OutputTo(new ByteArrayOutputStream()),
            new RedundantParentheses(
                s -> {
                    throw new IllegalStateException(
                        String.format("%s contains redundant parentheses", s)
                    );
                }
            )
        );
        if (correct) {
            syntax.parse();
        } else {
            Assertions.assertThrows(IllegalStateException.class, syntax::parse);
        }
    }

    static Stream<Arguments> testCases() {
        return Stream.of(
            Arguments.of("[] > foo\n  1.add (a.add 5) 4 > x", true),
            Arguments.of("[] > foo\n  add. 1 (a.add 5) > x", true),
            Arguments.of("[] > foo\n  add. 1 2 3 4 > x", true),
            Arguments.of("[] > foo\n  add. 1 2 3 4", true),
            Arguments.of("[] > foo\n  add 1 2 3 4", true),
            Arguments.of("[] > foo\n  1.add 4 > x", true),
            Arguments.of("[] > foo\n  (add 1).add 2", true),
            Arguments.of("[] > foo\n  1.add (1.add (1.add (1.add 1))) > x", true),
            Arguments.of("[] > foo\n  (1.add 1).add (1.add 1) > x", true),
            Arguments.of("[] > foo\n  add > x\n    1\n    1\n", true),
            Arguments.of("[] > foo\n  1.with-text \"(text) with parentheses a(n)...\" > x", true),
            Arguments.of("[] > foo\n  \"=(\" > x", true),
            Arguments.of("[] > foo\n  \"=)\" > x", true),
            Arguments.of("[] > foo\n  \")\" > x", true),
            Arguments.of("[] > foo\n  \"(-_-)\" > x", true),
            Arguments.of("[] > foo\n  \"Hello\".<.eq (\"Hello\".<)\n", true),
            Arguments.of("[] > foo\n  \"\"\"\n(-_-)\n\"\"\" > x", true),
            Arguments.of("[] > foo\n  add (-4) (-5)", true),
            Arguments.of("[] > foo\n  (1.add (a.add 5) (4)) > x", false),
            Arguments.of("[] > foo\n  (1.add (a.add 5) 4) > x", false),
            Arguments.of("[] > foo\n  (1.add (a.add 5) 4)", false),
            Arguments.of("[] > foo\n  1.add (5) > x", false),
            Arguments.of("[] > foo\n  (1.add 4) > x", false),
            Arguments.of("[] > foo\n  (1.add 4)", false),
            Arguments.of("[] > foo\n  ((1.add 1)) > x", false),
            Arguments.of("[] > foo\n  1.add 1 > x\n  (1.add 1)", false),
            Arguments.of("[] > foo\n  1.add 1 > x\n  (1.add 1) > y", false),
            Arguments.of("[] > foo\n  add > x\n    (1)\n    (1)\n", false)
        );
    }
}
