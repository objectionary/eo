/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test cases for checking of redundant parentheses.
 * The {@link org.eolang.parser.Syntax} object is used in order to check all possible expressions
 * and situations.
 *
 * @since 0.28.12
 *
 * @todo #2399:30min Do we need {@link RedundantParentheses} class? After refactoring grammar
 *  ({@see Program.g4}) parentheses are controlled at the level of grammar and can't be used in the
 *  many ways it was allowed to use them before. This is the reason the test is disabled. Need to
 *  check whether we really need the class with new grammar or not. If yes - return
 *  {@link RedundantParentheses} back to {@link XeListener} and refactor the test.
 *  If no - move the test cases below to {@link org.eolang.parser.typos} folder and remove
 *  {@link RedundantParentheses} class from the source code.
 */
class RedundantParenthesesTest {

    @ParameterizedTest
    @MethodSource("testCases")
    @Disabled
    void checksIfBracketsIsNotRedundant(
        final String program,
        final boolean correct
    ) throws IOException {
        final Syntax syntax = new Syntax(
            "foo",
            new InputOf(program),
            new OutputTo(new ByteArrayOutputStream())
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
            Arguments.of("# Comment().\n[] > obj\n", true),
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
