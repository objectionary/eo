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
                        new PhWith(
                            new PhCopy(
                                new PhMethod(
                                    new PhMethod(
                                        new PhMethod(
                                            Phi.Φ.attr("org").get(),
                                            "eolang"
                                        ),
                                        "io"
                                    ),
                                    "stdout"
                                )
                            ),
                            0,
                            new Data.ToPhi("Hello, world")
                        ),
                        "text"
                    )
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void takesStandardPackage() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhCopy(
                    new PhMethod(
                        new PhWith(
                            new PhCopy(Phi.Φ.attr("org.eolang.io.stdout").get()),
                            0, new Data.ToPhi("Hello, world")
                        ),
                        "text"
                    )
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void takesDirectly() {
        MatcherAssert.assertThat(
            new Dataized(
                Phi.Φ.attr("org").get().attr("eolang").get().attr("nan")
                    .get().attr("gt").get()
            ).take(Boolean.class),
            Matchers.equalTo(false)
        );
    }

    @Test
    void getsLocation() {
        MatcherAssert.assertThat(
            new PhWith(
                new PhLocated(
                    new PhMethod(
                        Phi.Φ,
                        "x"
                    ),
                    123,
                    56,
                    "Φ.org.eolang$obj"
                ),
                "Δ",
                new Data.Value<>("aaa")
            ).locator(),
            Matchers.equalTo("Φ.org.eolang$obj:123:56")
        );
    }
}
