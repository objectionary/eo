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
package org.eolang.maven.name;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link OnVersioned}.
 *
 * @since 0.30
 */
final class OnVersionedTest {

    @ParameterizedTest
    @CsvSource({
        "org.eolang.string, org.eolang.string#0.23.17",
        "org.eolang.dummy, org.eolang.dummy#0.23.19",
        "org.eolang.text, org.eolang.text#0.25.0",
        "org.eolang.aug, org.eolang.aug#0.25.5",
        "org.eolang.sept, org.eolang.sept#0.26.0",
        "org.eolang.oct, org.eolang.oct#0.27.0",
        "org.eolang.nov, org.eolang.nov#0.27.2",
        "org.eolang.dec, org.eolang.dec#0.28.0",
        "org.eolang.mon, org.eolang.mon#0.28.1",
        "org.eolang.tu, org.eolang.tu#0.28.10",
        "org.eolang.wen, org.eolang.wen#0.28.14",
        "org.eolang.th, org.eolang.th#0.28.2",
        "org.eolang.fri, org.eolang.fri#0.28.4",
        "org.eolang.sat, org.eolang.sat#0.28.5",
        "org.eolang.sun, org.eolang.sun#0.28.6",
        "org.eolang.penguin, org.eolang.penguin#0.28.7",
        "org.eolang.eagle, org.eolang.eagle#0.28.9"
    })
    @Disabled("Enable when OnVersioned.value() is implemented")
    void retrievesName(final String expected, final String origin) {
        final OnVersioned name = new OnVersioned(origin);
        MatcherAssert.assertThat(
            String.format("Can't retrieve object name from versioned object %s", origin),
            name.value(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    @CsvSource({
        "15c85d7f8cffe15b0deba96e90bdac98a76293bb, org.eolang.string#0.23.17",
        "4b19944d86058e3c81e558340a3a13bc335a2b48, org.eolang.dummy#0.23.19",
        "0aa6875c40d099c3f670e93d4134b629566c5643, org.eolang.text#0.25.0",
        "ff32e9ff70c2b3be75982757f4b0607dc37b258a, org.eolang.aug#0.25.5",
        "e0b783692ef749bb184244acb2401f551388a328, org.eolang.sept#0.26.0",
        "cc554ab82909eebbfdacd8a840f9cf42a99b64cf, org.eolang.oct#0.27.0",
        "00b60c7b2112cbad4e37ba96b162469a0e75f6df, org.eolang.nov#0.27.2",
        "6a70071580e95aeac104b2e48293d3dfe0669973, org.eolang.dec#0.28.0",
        "0c15066a2026cec69d613b709a301f1573f138ec, org.eolang.mon#0.28.1",
        "9b883935257bd59d1ba36240f7e213d4890df7ca, org.eolang.tu#0.28.10",
        "a7a4556bf1aa697324d805570f42d70affdddb75, org.eolang.wen#0.28.14",
        "54d83d4b1d28075ee623d58fd742eaa529febd3d, org.eolang.th#0.28.2",
        "6c6269d1f9a1c81ffe641538f119fe4e12706cb3, org.eolang.fri#0.28.4",
        "9c9352890b5d30e1b89c9147e7c95a90c9b8709f, org.eolang.sat#0.28.5",
        "17f89293e5ae6115e9a0234b754b22918c11c602, org.eolang.sun#0.28.6",
        "5f82cc1edffad67bf4ba816610191403eb18af5d, org.eolang.penguin#0.28.7",
        "be83d9adda4b7c9e670e625fe951c80f3ead4177, org.eolang.eagle#0.28.9"
    })
    @Disabled("Enable when OnVersioned.hash() is implemented")
    void retrievesHash(final String expected, final String origin) {
        final OnVersioned name = new OnVersioned(origin);
        MatcherAssert.assertThat(
            String.format("Can't retrieve object hash from versioned object %s", origin),
            name.hash().value(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    @Disabled("Enable when OnVersioned.toString() is implemented properly")
    void convertsToStringWithNonEmptyVersion() {
        final String string = "org.eolang.string#0.23.17";
        final OnVersioned name = new OnVersioned(string);
        MatcherAssert.assertThat(
            String.format("Can't convert versioned object %s to string", name),
            name.toString(),
            Matchers.equalTo("org.eolang.string#15c85d7f8cffe15b0deba96e90bdac98a76293bb")
        );
    }

    @Test
    void convertsToStringWithEmptyVersion() {
        final String string = "org.eolang.string";
        final OnVersioned name = new OnVersioned(string);
        MatcherAssert.assertThat(
            String.format("Can't convert versioned object %s to string", name),
            name.toString(),
            Matchers.equalTo(string)
        );
    }
}
