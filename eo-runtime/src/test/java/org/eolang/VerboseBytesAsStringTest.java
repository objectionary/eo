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

import java.nio.ByteBuffer;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link VerboseBytesAsString}.
 *
 * @since 0.1
 */
final class VerboseBytesAsStringTest {

    @ParameterizedTest
    @MethodSource("getTestSources")
    void representsString(final byte[] bytes, final String text) {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new VerboseBytesAsString(bytes).get(),
            Matchers.containsString(text)
        );
    }

    /**
     * Static method providing sources for parametrized test.
     * @return Stream of sources.
     */
    private static Stream<Arguments> getTestSources() {
        return Stream.of(
            Arguments.of(
                ByteBuffer.allocate(Double.BYTES).putDouble(12.345_67D).array(),
                "12.34567"
            ),
            Arguments.of(new byte[]{1}, "[1] = true"),
            Arguments.of(new byte[]{0}, "[0] = false"),
            Arguments.of(new byte[]{}, "[]"),
            Arguments.of(new byte[]{12}, "[12] = true"),
            Arguments.of(
                new byte[]{10, 11, 12, 13, 14, 15, 16, 17, -18, -19, -20, -21, 22},
                "0A0B0C0D 0E0F1011 EEEDECEB 16 = \"\\u000a\\u000b\\u000c\\u000d\\u000e\\u000f\\u0010\\u0011\\ufffd\\ufffd\\ufffd\\ufffd\\u0016\""
            )
        );
    }
}
