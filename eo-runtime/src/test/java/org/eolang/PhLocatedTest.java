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
        final Phi located = new PhLocated(new Data.ToPhi(0L), 123, 124, "qwerty");
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
                IllegalArgumentException.class,
                () -> new PhLocated(
                    new PhDefault() {
                        @Override
                        public byte[] delta() {
                            throw new IllegalArgumentException("oops");
                        }
                    },
                    10, 20
                ).delta(),
                "throws correct class"
            ),
            Matchers.hasToString(
                Matchers.containsString("Error in the \"?.Δ\" attribute at 10:20")
            )
        );
    }

}
