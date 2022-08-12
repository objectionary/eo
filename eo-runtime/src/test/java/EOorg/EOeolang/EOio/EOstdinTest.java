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

    private static final InputStream DEFAULT_STDIN = System.in;

    @AfterAll
    public static void restoreSystemInput() {
        System.setIn(DEFAULT_STDIN);
    }

    @Test
    public void nextLineOneLineTest() {
        String expected = "this is a test input!";
        mockSystemIn(expected + "\n");
        final Phi phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void nextLineMultiLineTest() {
        String expected = "this is a test input!";
        String input = expected + "\nanother line\nyet another line";
        mockSystemIn(input);
        final Phi phi = new PhMethod(new PhCopy(new EOstdin(Phi.Φ)), "next-line");
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void nextLineEmptyTest() {
        String expected = "";
        mockSystemIn(expected);
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
        String expected = "this is a testing input!\n";
        mockSystemIn(expected);
        final Phi phi = new PhCopy(new EOstdin(Phi.Φ));
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void stdinMultiLineTest() {
        String expected = "this is a test input!\nanother line\nyet another line";
        mockSystemIn(expected);
        final Phi phi = new PhCopy(new EOstdin(Phi.Φ));
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    @Test
    public void stdinEmptyTest() {
        String expected = "";
        mockSystemIn(expected);
        final Phi phi = new PhCopy(new EOstdin(Phi.Φ));
        final String actual = new Dataized(phi).take(String.class);
        MatcherAssert.assertThat(
            actual,
            Matchers.equalTo(expected)
        );
    }

    private void mockSystemIn(String mockingText) {
        System.setIn(new ByteArrayInputStream(mockingText.getBytes()));
    }
}
