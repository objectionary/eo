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
package org.eolang.maven.name;

import org.eolang.maven.hash.CommitHash;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link OnSwap}.
 *
 * @since 0.29.6
 */
class OnSwapTest {
    /**
     * First.
     */
    private static final String FIRST = new DelimitedName("stdout", "1234567").toString();

    /**
     * Second.
     */
    private static final String SECOND = new DelimitedName("sprintf", "7654321").toString();

    /**
     * Fake hash.
     */
    private static final CommitHash FAKE = new CommitHash.ChConstant("abcdef");

    @Test
    void behavesLikeFirst() {
        MatcherAssert.assertThat(
            String.format(
                "Swap object name should have been equal to %s, but it didn't",
                OnSwapTest.FIRST
            ),
            new OnSwap(
                true,
                new OnVersioned(OnSwapTest.FIRST, OnSwapTest.FAKE),
                new OnVersioned(OnSwapTest.SECOND, OnSwapTest.FAKE)
            ).toString(),
            Matchers.equalTo(OnSwapTest.FIRST)
        );
    }

    @Test
    void behavesLikeSecond() {
        MatcherAssert.assertThat(
            String.format(
                "Swap object name should have been equal to %s, but it didn't",
                OnSwapTest.SECOND
            ),
            new OnSwap(
                false,
                new OnVersioned(OnSwapTest.FIRST, OnSwapTest.FAKE),
                new OnVersioned(OnSwapTest.SECOND, OnSwapTest.FAKE)
            ).toString(),
            Matchers.equalTo(OnSwapTest.SECOND)
        );
    }

    @Test
    void behavesLikeUnversioned() {
        final String stdout = "stdout";
        MatcherAssert.assertThat(
            String.format(
                "Default swap object name should have been equal to %s, but it didn't",
                stdout
            ),
            new OnSwap(
                false,
                new OnVersioned(OnSwapTest.FIRST, OnSwapTest.FAKE)
            ).toString(),
            Matchers.equalTo(stdout)
        );
    }

    @Test
    void behavesLikeDefault() {
        MatcherAssert.assertThat(
            String.format(
                "Default swap object name should have been equal to %s, but it didn't",
                OnSwapTest.FIRST
            ),
            new OnSwap(
                true,
                new OnVersioned(OnSwapTest.FIRST, OnSwapTest.FAKE)
            ).toString(),
            Matchers.equalTo(OnSwapTest.FIRST)
        );
    }
}
