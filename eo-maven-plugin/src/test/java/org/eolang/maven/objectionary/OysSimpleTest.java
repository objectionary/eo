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
package org.eolang.maven.objectionary;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link OysSimple}.
 * @since 0.29.6
 */
class OysSimpleTest {
    @Test
    void putsIfAbsent() {
        final String hash = "abcdef";
        MatcherAssert.assertThat(
            String.format(
                "Objectionaries with objectionary by hash %s should have contained objectionary by that hash, but didn't",
                hash
            ),
            new OysSimple()
                .with(hash, new Objectionary.Fake())
                .has(hash),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotRewrite() {
        final String hash = "abcdef";
        MatcherAssert.assertThat(
            "Objectionaries should have not rewrite objectionary if it presents, but it did",
            new OysSimple()
                .with(hash, new Objectionary.Fake())
                .with(hash, new OyEmpty())
                .get(hash),
            Matchers.instanceOf(Objectionary.Fake.class)
        );
    }
}
