/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

/**
 * Test case for {@link EOarray}.
 *
 * @since 0.23
 */
public final class EOarrayEOeqTest {

    @ParameterizedTest
    @MethodSource("provideArgumentsForTests")
    public void areArraysEqual(final Object actual, final Object expected, final boolean result) {
        final Phi array1 = new Data.ToPhi(actual);
        final Phi array2 = new Data.ToPhi(expected);
        final Phi eq = array1.attr("eq").get();
        eq.attr(0).put(array2);
        MatcherAssert.assertThat(
            new Dataized(eq).take(Boolean.class),
            Matchers.equalTo(result)
        );
    }

    private static Stream<Arguments> provideArgumentsForTests() {
        return Stream.of(
            Arguments.of(
                new Phi[] {
                    new Data.ToPhi("str")
                },
                new Phi[] {
                    new Data.ToPhi("str")
                }, true),
            Arguments.of(
                new Phi[] {
                    new Data.ToPhi("str")
                },
                new Phi[] {
                    new Data.ToPhi("wrong")
                }, false),
            Arguments.of(
                new Phi[] {
                    new Data.ToPhi("str")
                },
                new Phi[] {
                    new Data.ToPhi("str"),
                    new Data.ToPhi("str2")
                }, false),
            Arguments.of(
                new Phi[] {
                    new Data.ToPhi("str")
                },
                "str", false)
        );
    }
}
