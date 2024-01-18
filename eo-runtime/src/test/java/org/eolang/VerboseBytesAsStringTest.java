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

import java.util.Arrays;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link VerboseBytesAsString}.
 *
 * @since 0.1
 */
public final class VerboseBytesAsStringTest {

    @ParameterizedTest
    @MethodSource("getTestSources")
    void representsString(final Object object) {
        MatcherAssert.assertThat(
            new VerboseBytesAsString(VerboseBytesAsStringTest.toBytes(object)).get(),
            Matchers.containsString(
                new VerboseBytesAsStringTest.ArgumentsUtils().toString(object)
            )
        );
    }

    /**
     * Makes byte array from Object.
     * @param object Object.
     * @return Array of bytes.
     */
    private static byte[] toBytes(final Object object) {
        final Bytes ret;
        if (object instanceof String) {
            ret = new BytesOf((String) object);
        } else if (object instanceof byte[]) {
            ret = new BytesOf((byte[]) object);
        } else if (object instanceof Long) {
            ret = new BytesOf((Long) object);
        } else if (object instanceof Double) {
            ret = new BytesOf((Double) object);
        } else if (object instanceof Boolean) {
            if ((Boolean) object) {
                ret = new BytesOf(new byte[]{0x1});
            } else {
                ret = new BytesOf(new byte[]{0x0});
            }
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "Unsupported class of argument: %s",
                    object.getClass()
                )
            );
        }
        return ret.take();
    }

    /**
     * Static method providing sources for parametrized test.
     * @return Stream of sources.
     */
    private static Stream<Object> getTestSources() {
        return new VerboseBytesAsStringTest.ArgumentsUtils().getTestSources();
    }

    /**
     * Just a class providing resources for tests.
     *
     * @since 0.1
     */
    public static class ArgumentsUtils {

        /**
         * Makes a readable string from object.
         * @param object Object.
         * @return String.
         */
        public String toString(final Object object) {
            final String ret;
            if (object instanceof byte[]) {
                ret = Arrays.toString((byte[]) object);
            } else {
                ret = object.toString();
            }
            return ret;
        }

        /**
         * Input arguments for unit tests.
         *
         * @return Stream of arguments.
         */
        public Stream<Object> getTestSources() {
            return Stream.of(
                12345L,
                "qwerty",
                12.34567D,
                true,
                false,
                new byte[]{},
                new byte[]{12},
                new byte[]{10, 11, 12, 13, 14, 15, 16, 17},
                new byte[]{10, 11, 12, 13, 14, 15, 16, 17, -18, -19, -20, -21, 22}
            );
        }
    }

}
