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

import EOorg.EOeolang.EOseq;
import EOorg.EOeolang.EOtuple$EOempty;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.eolang.AtComposite;
import org.eolang.AtCompositeTest;
import org.eolang.AtOnce;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link EOstdout}.
 * @since 0.1
 */
public final class EOstdoutTest {
    @Test
    public void printsFromTuple() {
        final Phi tuple = Phi.Φ.take("org").take("eolang").take("tuple");
        final Phi copy = tuple.copy();
        copy.put(0, tuple.take("empty"));
        copy.put(1, new Data.ToPhi("Hello"));
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final Phi ret = copy.take("at").copy();
        ret.put(0, new Data.ToPhi(0L));
        final Phi stdout = new EOstdout(new PrintStream(stream));
        stdout.put(0, ret);
        new Dataized(stdout).asBool();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            stream.toString(),
            Matchers.equalTo("Hello")
        );
    }

    @Test
    public void printsString() {
        final Phi format = new Data.ToPhi("Hello, world!\n");
        final Phi phi = new PhWith(
            new PhCopy(new EOstdout()),
            "text",
            format
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(phi).asBool(),
            Matchers.equalTo(true)
        );
    }

    @ParameterizedTest
    @CsvSource({"lt", "gt", "lte", "gte", "eq"})
    public void doesNotPrintTwiceOnIntComparisonMethods(final String method) {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final String str = "Hello world";
        new Dataized(
            new PrintWithCmp(
                new PhMethod(
                    new Data.ToPhi(1L),
                    method
                ),
                new Data.ToPhi(2L),
                new PhWith(
                    new EOstdout(new PrintStream(stream)),
                    "text",
                    new Data.ToPhi(str)
                )
            )
        ).take();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            stream.toString(),
            Matchers.equalTo(str)
        );
    }

    @ParameterizedTest()
    @CsvSource({"lt", "gt", "lte", "gte"})
    public void doesNotPrintTwiceOnFloatComparisonMethods(final String method) {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final String str = "Hello world";
        new Dataized(
            new PrintWithCmp(
                new PhMethod(
                    new Data.ToPhi(1.0),
                    method
                ),
                new Data.ToPhi(3.0),
                new PhWith(
                    new EOstdout(new PrintStream(stream)),
                    "text",
                    new Data.ToPhi(str)
                )
            )
        ).take();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            stream.toString(),
            Matchers.equalTo(str)
        );
    }

    /**
     * PrintWithCmp Phi.
     *
     * @since 1.0
     */
    private static class PrintWithCmp extends PhDefault {
        /**
         * Ctor.
         * E.g.
         * 1.lt
         *  seq
         *    *
         *      stdout "Hello world"
         *      3
         *
         * @param method Comparison PhMethod ("lt", "gt", "lte", "gte")
         * @param value Phi value to be compared
         * @param stdout Phi object with printing a string via {@link EOstdout} object
         */
        PrintWithCmp(final Phi method, final Phi value, final Phi stdout) {
            super();
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        self -> new Data.ToPhi(
                            new Dataized(
                                new PhWith(
                                    method,
                                    0,
                                    new PhWith(
                                        new EOseq(),
                                        0,
                                        new PhWith(
                                            new PhWith(
                                                new EOtuple$EOempty()
                                                    .take("with")
                                                    .copy(),
                                                0,
                                                stdout
                                            ).take("with").copy(),
                                            0, value
                                        )
                                    )
                                )
                            ).take()
                        )
                    )
                )
            );
        }
    }
}
