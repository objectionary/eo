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
package org.eolang.maven.name;

import org.eolang.maven.hash.CommitHash;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link OnVersioned}.
 *
 * @since 0.29.6
 */
final class OnVersionedTest {
    /**
     * Object to test.
     */
    private static final String OBJECT = "stdout|1234567";

    /**
     * Fake hash.
     */
    private static final CommitHash FAKE = new CommitHash.ChConstant("abcdefg");

    @Test
    void returnsFullNameWithVersions() {
        MatcherAssert.assertThat(
            String.format(
                "Versioned object %s as string should have been equal to %s, but it didn't",
                OnVersionedTest.OBJECT,
                OnVersionedTest.OBJECT
            ),
            new OnVersioned(
                new OnDefault(OnVersionedTest.OBJECT, OnVersionedTest.FAKE),
                true
            ).asString(),
            Matchers.equalTo(OnVersionedTest.OBJECT)
        );
    }

    @Test
    void returnsNameWithoutVersions() {
        final String stdout = "stdout";
        MatcherAssert.assertThat(
            String.format(
                "Not versioned object %s as string should have been equal to %s, but it didn't",
                OnVersionedTest.OBJECT,
                stdout
            ),
            new OnVersioned(
                new OnDefault(OnVersionedTest.OBJECT, OnVersionedTest.FAKE),
                false
            ).asString(),
            Matchers.equalTo(stdout)
        );
    }
}
