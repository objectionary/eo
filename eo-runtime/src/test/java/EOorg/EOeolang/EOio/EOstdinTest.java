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

/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package EOorg.EOeolang.EOio;

import EOorg.EOeolang.EOerror;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOstdin}.
 *
 * @since 0.23
 */
public final class EOstdinTest {

    /**
     * DEFAULT_STDIN.
     */
    private static final InputStream DEFAULT_STDIN = System.in;

    @AfterAll
    public static void restoreSystemInput() {
        System.setIn(DEFAULT_STDIN);
    }

    @Test
    public void nextLineOneLineTest() {
        final String expected = "this is a test input!";
        this.mockSystemIn(String.format("%s\n", expected));
        final Phi phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void nextLineMultiLineTest() {
        final String expected = "this is a test input!";
        final String input = String.format("%s\nanother line\nyet another line", expected);
        this.mockSystemIn(input);
        final Phi phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void nextLineEmptyTest() {
        final String expected = "";
        this.mockSystemIn(expected);
        final Phi phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        final EOerror.ExError error = Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(phi).take(String.class)
        );
        MatcherAssert.assertThat(
            new Dataized(error.enclosure()).take(String.class),
            Matchers.containsString(
                "There is no line in the standard input stream to consume"
            )
        );
    }

    @Test
    public void stdinOneLineTest() {
        final String expected = "this is a testing input!\n";
        this.mockSystemIn(expected);
        final Phi phi = new PhCopy(new EOstdin(Phi.Φ));
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void stdinMultiLineTest() {
        final String expected = "this is a test input!\nanother line\nyet another line";
        this.mockSystemIn(expected);
        final Phi phi = new PhCopy(new EOstdin(Phi.Φ));
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void stdinEmptyTest() {
        final String expected = "";
        this.mockSystemIn(expected);
        final Phi phi = new PhCopy(new EOstdin(Phi.Φ));
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void stdinfewOneLineTest() {
        final String first = "\u0066\u0069\u0072\u0073\u0074";
        final String second = "\u0073\u0065\u0063\u006F\u006E\u0064";
        final String third = "\u0074\u0068\u0069\u0072\u0064";
        this.mockSystemIn("first\nsecond\nthird\n");
        Phi phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(first)
        );
        phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(second)
        );
        phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(third)
        );
    }

    private void mockSystemIn(final String text) {
        System.setIn(new ByteArrayInputStream(text.getBytes()));
    }
}
