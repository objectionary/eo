/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

package org.eolang;

import EOorg.EOeolang.EOchar;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link Param}.
 *
 * @since 1.0
 */
public final class ParamTest {

    @CsvSource({
        "A,   java.lang.Byte, 65",
        "B,   java.lang.Double, 66.0",
        "Ф,   java.lang.Byte, 36",
        "Ф,   java.lang.Long, 1060",
    })
    @ParameterizedTest
    void handlesChar(
        final Character num,
        final String type,
        final String result

    ) throws ClassNotFoundException {
        MatcherAssert.assertThat(
            new Param(
                new PhWith(
                    new EOchar(Phi.Φ),
                    "ρ",
                    new Data.Value<>(num)
                )
            ).numeric(Class.forName(type)),
            Matchers.hasToString(result)
        );
    }

    @CsvSource({
        "0,   java.lang.Byte, 0",
        "-1,  java.lang.Byte, -1",
        "128,  java.lang.Byte, -128",
        "65,  java.lang.Character, A",
        "NaN,  java.lang.Double, NaN",
        "1234.0,  java.lang.Integer, 1234",
        "123.4,  java.lang.Float, 123.4",
        "1.1,  java.lang.Double, 1.1",
    })
    @ParameterizedTest
    void handlesNumbers(
        final String num,
        final String type,
        final String result

    ) throws ClassNotFoundException {
        MatcherAssert.assertThat(
            new Param(
                new PhNumber(num)
            ).numeric(Class.forName(type)),
            Matchers.hasToString(result)
        );
    }

    private static class PhNumber extends PhDefault {
        PhNumber(final String text) {
            super(Phi.Φ);
            this.add(
                "ρ",
                new AtOnce(
                    new AtComposite(this, rho ->
                        new Data.ToPhi(
                            Double.parseDouble(text)
                        )
                    )
                )
            );
        }
    }

}