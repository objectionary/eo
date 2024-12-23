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
package org.eolang;

import EOorg.EOeolang.EOerror;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhLocatedTest}.
 *
 * @since 0.36.0
 */
final class PhLocatedTest {

    @Test
    void savesLocationAfterCopying() {
        final Phi located = new PhLocated(new Data.ToPhi(0L), "foo", 123, 124, "qwerty");
        MatcherAssert.assertThat(
            "saves location",
            located.copy().locator(),
            Matchers.equalTo(located.locator())
        );
    }

    @Test
    void catchesRuntimeException() {
        MatcherAssert.assertThat(
            "rethrows correctly",
            Assertions.assertThrows(
                EOerror.ExError.class,
                () -> new PhLocated(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new IllegalArgumentException("oops");
                        }
                    }
                ).delta(),
                "throws correct class"
            ).messages(),
            Matchers.hasItem(
                Matchers.containsString("Error in \"?.Δ\" at unknown:0:0")
            )
        );
    }

    @Test
    void rendersMultiLayeredErrorMessageCorrectly() {
        MatcherAssert.assertThat(
            "rethrows correctly",
            Assertions.assertThrows(
                EOerror.ExError.class,
                () -> new PhLocated(
                    new PhWith(
                        new EOerror(),
                        "message",
                        new Data.ToPhi("oops")
                    )
                ).take("foo"),
                "throws correct class"
            ),
            Matchers.hasToString(
                Matchers.containsString("Δ = [0x6F6F7073-] = \"oops\"")
            )
        );
    }

    @Test
    void showsFileNameAndLineNumber() {
        MatcherAssert.assertThat(
            "shows file name and line number",
            new Dataized(
                Assertions.assertThrows(
                    EOerror.ExError.class,
                    () -> new PhLocated(
                        new PhDefault() {
                            @Override
                            public Phi take(final String name) {
                                throw new IllegalArgumentException("intentional error");
                            }
                        }
                    ).take("foo"),
                    "throws correct class"
                ).enclosure()
            ).take(String.class),
            Matchers.equalTo("intentional error")
        );
    }

}
