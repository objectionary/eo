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
 * Test for {@link Expect}.
 *
 * @since 0.1.0
 */
final class ExpectTest {

    @Test
    void buildsAndChecks() {
        MatcherAssert.assertThat(
            "passes through and throws correctly",
            new Expect<>("something", () -> 42)
                .must(i -> i > 0)
                .otherwise("must be positive")
                .that(i -> Integer.toString(i))
                .must(s -> !s.isEmpty())
                .it(),
            Matchers.equalTo("42")
        );
    }

    @Test
    void failsWithCorrectTrace() {
        MatcherAssert.assertThat(
            "error message is correct",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Expect<>("a number", () -> 42)
                    .must(i -> i < 0)
                    .otherwise("must be negative")
                    .it(),
                "fails on check"
            ).getMessage(),
            Matchers.containsString("negative")
        );
    }
}
