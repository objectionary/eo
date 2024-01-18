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
package org.eolang.maven.hash;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link ChPattern}.
 *
 * @since 0.28.11
 */
class ChPatternTest {

    /**
     * Get hash by tag using pattern test.
     *
     * @param pattern Pattern
     * @param tag Tag
     * @param expected Expected Hash
     */
    @ParameterizedTest
    @CsvSource({
        "'0.*.*:abc2sd3', '0.0.0', abc2sd3",
        "'*.*.*:abc2sd3', '2.2.2', abc2sd3",
        "'0.*.*:abc2sd3', '1.0.0', ''",
        "'0.*.*:m23ss3h', '0.1.2', 'm23ss3h'",
        "'0.*.*:m23ss3h,1.*.*:abc2sd3', '0.1.2', 'm23ss3h'",
        "'0.*.*:m23ss3h, 1.*.*:abc2sd3', '0.1.2', 'm23ss3h'",
        "'0.*.*:m23ss3h,1.*.*:abc2sd3', '1.1.2', 'abc2sd3'",
        "'0.*.*:m23ss3h,3.*.*:abc2sd3', '2.1.2', ''",
        "'3.*.*:m23ss3h,3.1.*:abc2sd3', '3.1.2', 'abc2sd3'",
        "'3.1.2:m23ss3h,3.1.*:abc2sd3', '3.1.2', 'm23ss3h'",
        "'master:m23ss3h,3.1.*:abc2sd3', 'master', 'm23ss3h'",
        "'master:m23ss3h,composite-tag:abc2sd3', 'composite-tag', 'abc2sd3'"
    })
    void returnsCorrectHashByPattern(
        final String pattern,
        final String tag,
        final String expected
    ) {
        MatcherAssert.assertThat(new ChPattern(pattern, tag).value(), Matchers.equalTo(expected));
    }
}
