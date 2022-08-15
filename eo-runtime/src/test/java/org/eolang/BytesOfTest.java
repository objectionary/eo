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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link BytesOf}.
 *
 * @since 1.0
 */
final class BytesOfTest {

    @Test
    void notWorks() {
        final String text = "abc";
        final Bytes bytes = new BytesOf(text);
        MatcherAssert.assertThat(
            bytes.not().not(),
            Matchers.equalTo(new BytesOf(text))
        );
    }

    @Test
    void notWorksNumeric() {
        final Bytes bytes = new BytesOf(-128L);
        MatcherAssert.assertThat(
            bytes.not(),
            Matchers.equalTo(new BytesOf(127L))
        );
    }
    @Test
    void andWorks() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            bytes.and(bytes.not()),
            Matchers.equalTo(new BytesOf(0L))
        );
    }

    @Test
    void orWorks() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            bytes.or(bytes.not()),
            Matchers.equalTo(new BytesOf(-1L))
        );
    }

    @Test
    void xorWorks() {
        final Bytes bytes = new BytesOf(512L);
        MatcherAssert.assertThat(
            bytes.xor(new BytesOf(-512L)),
            Matchers.equalTo(new BytesOf(-1024L))
        );
    }

    @Test
    void asNumberLong() {
        final Bytes bytes = new BytesOf(512L);
        MatcherAssert.assertThat(
            bytes.asNumber(Long.class),
            Matchers.equalTo(512L)
        );
    }

    @Test
    void underflowForLong() {
        final Bytes bytes = new BytesOf("A");
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            () -> bytes.asNumber(Long.class)
        );
    }
}