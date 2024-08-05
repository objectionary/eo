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

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOio; // NOPMD

import org.eolang.AtCompositeTest;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.StdIo;

/**
 * Test case for {@link EOstdin}.
 * {@link EOstdin} is the generated class. This is the reason
 * why we disable jtcop check.
 *
 * @since 0.23
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOstdinTest {

    /**
     * Next line.
     */
    private static final String NEXT_LINE = "next-line";

    @StdIo("this is a test input!")
    @Test
    void dataizesNextLineOneLine() {
        final String expected = "this is a test input!";
        final Phi phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(expected)
        );
    }

    @StdIo("this is a testing input!")
    @Test
    void dataizesStdinOneLine() {
        final String expected = "this is a testing input!";
        final Phi phi = new PhCopy(new EOstdin());
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(expected)
        );
    }

    @StdIo({"this is a test input!", "another line", "yet another line"})
    @Test
    void dataizesNextLineMultiLine() {
        final String expected = "this is a test input!";
        final Phi phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(expected)
        );
    }

    @StdIo("")
    @Test
    void dataizesNextLineEmpty() {
        final Phi phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        final String input = Assertions.assertDoesNotThrow(
            () -> new Dataized(phi).asString(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            input,
            Matchers.equalTo("")
        );
    }

    @StdIo("")
    @Test
    void dataizesEmptyStdin() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(new EOstdin()).asString(),
            Matchers.equalTo("")
        );
    }

    @StdIo({"this is a test input!", "another line", "yet another line"})
    @Test
    void dataizesStdinMultiLine() {
        final String first = "this is a test input!";
        final String second = "another line";
        final String third = "yet another line";
        final Phi phi = new PhCopy(new EOstdin());
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(String.join(System.lineSeparator(), first, second, third))
        );
    }

    @StdIo({"first", "second", "third"})
    @Test
    void dataizesStdinFewOneLine() {
        final String first = "\u0066\u0069\u0072\u0073\u0074";
        final String second = "\u0073\u0065\u0063\u006F\u006E\u0064";
        final String third = "\u0074\u0068\u0069\u0072\u0064";
        Phi phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(first)
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(second)
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(third)
        );
    }

    @StdIo({"first", "", "third"})
    @Test
    void dataizesStdinEmptyLineBetweenNonEmpty() {
        final String first = "\u0066\u0069\u0072\u0073\u0074";
        final String third = "\u0074\u0068\u0069\u0072\u0064";
        Phi phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(first)
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo("")
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), EOstdinTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(third)
        );
    }
}
