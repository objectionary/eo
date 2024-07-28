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
 */
package EOorg.EOeolang.EOio;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.StdIn;
import org.junitpioneer.jupiter.StdIo;
import org.junitpioneer.jupiter.StdOut;

/**
 * Test case for {@link EOconsole}.
 * @since 0.39
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOconsoleTest {
    @Test
    @StdIo
    void printsFromTuple(final StdOut output) {
        final Phi tuple = Phi.Φ.take("org").take("eolang").take("tuple");
        final Phi copy = tuple.copy();
        copy.put(0, tuple.take("empty"));
        copy.put(1, new Data.ToPhi("Hello"));
        final Phi ret = copy.take("at").copy();
        ret.put(0, new Data.ToPhi(0L));
        final Phi written = new EOconsole$EOwrite$EOwritten_bytes();
        written.put(0, ret);
        new Dataized(written).asBool();
        MatcherAssert.assertThat(
            "The `console.write.written-bytes` object should have printed string from `tuple`, but it didn't",
            output.capturedString(),
            Matchers.equalTo("Hello")
        );
    }

    @Test
    void dataizesAsTrue() {
        final Phi phi = new PhWith(
            new PhCopy(new EOconsole$EOwrite$EOwritten_bytes()),
            "buffer",
            new Data.ToPhi("Hello, world!\n")
        );
        Assertions.assertTrue(
            new Dataized(phi).asBool(),
            "The `console.write.written-bytes` should have returned TRUE, but it didn't"
        );
    }

    @Test
    @StdIo
    void writesToConsoleSequentially(final StdOut output) {
        final Phi console = Phi.Φ.take("org.eolang.io.console");
        final Phi buffer = new Data.ToPhi("Hello");
        final Phi first = new PhWith(
            new PhCopy(
                new PhMethod(console, "write")
            ),
            0, buffer
        );
        final Phi second = new PhWith(
            new PhCopy(
                new PhMethod(first, "write")
            ),
            0, buffer
        );
        new Dataized(second).take();
        MatcherAssert.assertThat(
            "The `console.write` object should have return output block ready to write again, but it didn't",
            output.capturedString(),
            Matchers.equalTo("HelloHello")
        );
    }

    @Test
    @StdIo("Hello, world!")
    void readsBytesFromStandardInput(final StdIn input) {
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read all bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(13)
                )
            ).asString(),
            Matchers.equalTo("Hello, world!")
        );
    }

    @Test
    @StdIo("Hello")
    void readsOnlyAvailableBytes(final StdIn input) {
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read only available bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(10)
                )
            ).asString(),
            Matchers.equalTo("Hello\n")
        );
    }

    @Test
    @StdIo("")
    void readsOnlyNewLineFromEmptyInput() {
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have returned empty bytes read from empty input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(10)
                )
            ).asString(),
            Matchers.equalTo("\n")
        );
    }

    @Test
    @StdIo("Hello world")
    void readsByPortionsFromInput() {
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read first 5 bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(5)
                )
            ).asString(),
            Matchers.equalTo("Hello")
        );
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read second 5 bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(6)
                )
            ).asString(),
            Matchers.equalTo(" world")
        );
    }

    @Test
    @StdIo("Hello world")
    void readsSequentiallyFromInputBlock() {
        final Phi console = Phi.Φ.take("org.eolang.io.console");
        final Phi first = new PhWith(
            new PhCopy(
                new PhMethod(console, "read")
            ),
            0, new Data.ToPhi(5)
        );
        final Phi second = new PhWith(
            new PhCopy(
                new PhMethod(first, "read")
            ),
            0, new Data.ToPhi(6)
        );
        MatcherAssert.assertThat(
            "The `console.read` object should have return input block ready to `read` again, but it didn't",
            new Dataized(second).asString(),
            Matchers.equalTo(" world")
        );
    }
}
