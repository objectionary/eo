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
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Phi}.
 *
 * @since 0.22
 */
final class PhiTest {

    @Test
    void takesPackage() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhCopy(
                    new PhMethod(
                        new PhMethod(
                            new PhMethod(
                                new PhMethod(
                                    Phi.Φ.attr("org").get(),
                                    "eolang"
                                ),
                                "math"
                            ),
                            "random"
                        ),
                        "pseudo"
                    )
                )
            ).take(Double.class),
            Matchers.greaterThan(-1.0d)
        );
    }

    @Test
    void takesStandardPackage() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhCopy(
                    new PhMethod(
                        Phi.Φ.attr("org.eolang.math.random").get(), "pseudo"
                    )
                )
            ).take(Double.class),
            Matchers.greaterThan(-1.0d)
        );
    }

    @Test
    void takesDirectly() {
        MatcherAssert.assertThat(
            new Dataized(
                Phi.Φ.attr("org").get().attr("eolang").get().attr("math")
                    .get().attr("random").get().attr("pseudo").get()
            ).take(Double.class),
            Matchers.greaterThan(-1.0d)
        );
    }

    @Test
    void locationTest() {
        MatcherAssert.assertThat(
            new PhWith(
                new PhLocated(
                    new PhMethod(
                        Phi.Φ,
                        "x"
                    ),
                    123,
                    56
                ),
                "Δ",
                new Data.Value<>("aaa")
            ).locator(),
            Matchers.equalTo("123:56")
        );
    }
}
