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
package org.eolang.maven.name;

import org.eolang.maven.hash.CommitHashesMap;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link OnReplaced}.
 *
 * @since 0.30
 */
final class OnReplacedTest {
    /**
     * String.
     */
    private static final String STRING = "org.eolang.string";

    @ParameterizedTest
    @CsvSource({
        "org.eolang.string, org.eolang.string|0.23.17",
        "org.eolang.dummy, org.eolang.dummy|0.23.19",
        "org.eolang.text, org.eolang.text|0.25.0",
        "org.eolang.aug, org.eolang.aug|0.25.5",
        "org.eolang.sept, org.eolang.sept|0.26.0",
        "org.eolang.oct, org.eolang.oct|0.27.0",
        "org.eolang.nov, org.eolang.nov|0.27.2",
        "org.eolang.dec, org.eolang.dec|0.28.0",
        "org.eolang.mon, org.eolang.mon|0.28.1",
        "org.eolang.tu, org.eolang.tu|0.28.10",
        "org.eolang.wen, org.eolang.wen|0.28.14",
        "org.eolang.th, org.eolang.th|0.28.2",
        "org.eolang.fri, org.eolang.fri|0.28.4",
        "org.eolang.sat, org.eolang.sat|0.28.5",
        "org.eolang.sun, org.eolang.sun|0.28.6",
        "org.eolang.penguin, org.eolang.penguin|0.28.7",
        "org.eolang.eagle, org.eolang.eagle|0.28.9"
    })
    void retrievesName(final String expected, final String origin) {
        final ObjectName name = new OnReplaced(origin);
        MatcherAssert.assertThat(
            String.format("Can't retrieve object name from %s, versioned object %s", name, origin),
            name.value(),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "15c85d7, org.eolang.string|0.23.17",
        "4b19944, org.eolang.dummy|0.23.19",
        "0aa6875, org.eolang.text|0.25.0",
        "ff32e9f, org.eolang.aug|0.25.5",
        "e0b7836, org.eolang.sept|0.26.0",
        "cc554ab, org.eolang.oct|0.27.0",
        "00b60c7, org.eolang.nov|0.27.2",
        "6a70071, org.eolang.dec|0.28.0",
        "0c15066, org.eolang.mon|0.28.1",
        "9b88393, org.eolang.tu|0.28.10",
        "a7a4556, org.eolang.wen|0.28.14",
        "54d83d4, org.eolang.th|0.28.2",
        "6c6269d, org.eolang.fri|0.28.4",
        "9c93528, org.eolang.sat|0.28.5",
        "17f8929, org.eolang.sun|0.28.6",
        "5f82cc1, org.eolang.penguin|0.28.7",
        "be83d9a, org.eolang.eagle|0.28.9"
    })
    void retrievesHash(final String expected, final String origin) {
        final ObjectName name = new OnReplaced(origin, new CommitHashesMap.Fake());
        MatcherAssert.assertThat(
            String.format("Can't retrieve object hash from %s versioned object %s", name, origin),
            name.hash().value(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void convertsToStringWithNonEmptyVersion() {
        final ObjectName name = new OnReplaced(
            new DelimitedName(OnReplacedTest.STRING, "0.23.17").toString(),
            new CommitHashesMap.Fake()
        );
        MatcherAssert.assertThat(
            String.format("Can't convert versioned object %s to string", name),
            name.toString(),
            Matchers.equalTo(
                new DelimitedName(OnReplacedTest.STRING, "15c85d7").toString()
            )
        );
    }

    @Test
    void convertsToStringWithEmptyVersion() {
        final ObjectName name = new OnReplaced(OnReplacedTest.STRING);
        MatcherAssert.assertThat(
            String.format("Can't convert versioned object %s to string", name),
            name.toString(),
            Matchers.equalTo(OnReplacedTest.STRING)
        );
    }

    @Test
    void convertsToStringFromOtherObjectName() {
        final ObjectName name = new OnReplaced(
            new OnVersioned(
                OnReplacedTest.STRING,
                "0.28.5"
            )
        );
        MatcherAssert.assertThat(
            String.format("Couldn't convert versioned object %s to string", name),
            name.toString(),
            Matchers.equalTo(
                new DelimitedName(OnReplacedTest.STRING, "9c93528").toString()
            )
        );
    }
}
