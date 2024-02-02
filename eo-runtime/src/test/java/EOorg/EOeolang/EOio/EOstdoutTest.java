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
import EOorg.EOeolang.EOtuple;
import EOorg.EOeolang.EOtuple$EOempty;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
        final Phi tuple = new EOtuple(Phi.Φ);
        tuple.attr(0).put(
            Phi.Φ.attr("org").get()
                .attr("eolang").get()
                .attr("tuple").get()
                .attr("empty").get()
        );
        tuple.attr(1).put(new Data.ToPhi("Hello"));
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        new Dataized(
            new PhWith(
                new EOstdout(Phi.Φ, new PrintStream(stream)),
                0,
                new PhWith(
                    tuple.attr("at").get().copy(),
                    0, new Data.ToPhi(0L)
                )
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            stream.toString(),
            Matchers.equalTo("Hello")
        );
    }

    @Test
    public void printsString() {
        final Phi format = new Data.ToPhi("Hello, world!\n");
        final Phi phi = new PhWith(
            new PhCopy(new EOstdout(Phi.Φ)),
            "text",
            format
        );
        MatcherAssert.assertThat(
            new Dataized(phi).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @ParameterizedTest
    @CsvSource({"lt", "gt", "lte", "gte", "eq"})
    public void doesNotPrintTwiceOnIntComparisonMethods(final String method) {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final String str = "Hello world";
        new Dataized(
            new PhWith(
                new PhMethod(
                    new Data.ToPhi(1L),
                    method
                ),
                0,
                new PhWith(
                    new EOseq(Phi.Φ),
                    0,
                    new PhWith(
                        new PhWith(
                            new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                            0,
                            new PhWith(
                                new EOstdout(Phi.Φ, new PrintStream(stream)),
                                "text",
                                new Data.ToPhi(str)
                            )
                        ).attr("with").get().copy(),
                        0, new Data.ToPhi(2L)
                    )
                )
            )
        ).take();
        MatcherAssert.assertThat(
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
            new PhWith(
                new PhMethod(
                    new Data.ToPhi(1.0),
                    method
                ),
                0,
                new PhWith(
                    new EOseq(Phi.Φ),
                    0,
                    new PhWith(
                        new PhWith(
                            new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                            0,
                            new PhWith(
                                new EOstdout(Phi.Φ, new PrintStream(stream)),
                                "text",
                                new Data.ToPhi(str)
                            )
                        ).attr("with").get().copy(),
                        0, new Data.ToPhi(3.0)
                    )
                )
            )
        ).take();
        MatcherAssert.assertThat(
            stream.toString(),
            Matchers.equalTo(str)
        );
    }
}
