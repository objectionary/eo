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
 * Test cases for {@link OnDefault}.
 *
 * @since 0.29.6
 */
final class OnDefaultTest {
    /**
     * Stdout.
     */
    private static final String STDOUT = "stdout";

    /**
     * Hash for {@code STDOUT}.
     */
    private static final String HASH = "1234567";

    /**
     * Test object.
     */
    private static final String OBJECT = String.join(
        "|",
        OnDefaultTest.STDOUT,
        OnDefaultTest.HASH
    );

    /**
     * Fake commit hash.
     */
    private static final CommitHash FAKE = new CommitHash.ChConstant("abcdefg");

    @Test
    void returnsTheSameValidFullName() {
        MatcherAssert.assertThat(
            String.format(
                "Object name %s as string should have not been changed, but it did",
                OnDefaultTest.OBJECT
            ),
            new OnDefault(OnDefaultTest.OBJECT, OnDefaultTest.FAKE).toString(),
            Matchers.equalTo(OnDefaultTest.OBJECT)
        );
    }

    @Test
    void takesNameFromGivenFullName() {
        MatcherAssert.assertThat(
            String.format(
                "Name of object %s should have been equal to %s, but it didn't",
                OnDefaultTest.OBJECT,
                OnDefaultTest.STDOUT
            ),
            new OnDefault(
                OnDefaultTest.OBJECT,
                OnDefaultTest.FAKE
            ).value(),
            Matchers.equalTo(OnDefaultTest.STDOUT)
        );
    }

    @Test
    void takesHashFromGivenFullName() {
        MatcherAssert.assertThat(
            String.format(
                "Hash of object %s should have been equal to %s, but it didn't",
                OnDefaultTest.OBJECT,
                OnDefaultTest.HASH
            ),
            new OnDefault(
                OnDefaultTest.OBJECT,
                OnDefaultTest.FAKE
            )
                .hash()
                .value(),
            Matchers.equalTo(OnDefaultTest.HASH)
        );
    }

    @Test
    void buildsFullNameWithGivenDefaultHash() {
        final String built = String.join(
            "|",
            OnDefaultTest.STDOUT,
            OnDefaultTest.FAKE.value()
        );
        MatcherAssert.assertThat(
            String.format(
                "Object %s as string with fake hash %s should have been equal to %s, but it didn't",
                OnDefaultTest.STDOUT,
                OnDefaultTest.FAKE,
                built
            ),
            new OnDefault(
                OnDefaultTest.STDOUT,
                OnDefaultTest.FAKE
            ).toString(),
            Matchers.equalTo(built)
        );
    }

    @Test
    void takesHashFromGivenDefaultHash() {
        MatcherAssert.assertThat(
            String.format(
                "Hash of object %s should have been equal to default hash %s, but it didn't",
                OnDefaultTest.STDOUT,
                OnDefaultTest.FAKE
            ),
            new OnDefault(
                OnDefaultTest.STDOUT,
                OnDefaultTest.FAKE
            )
                .hash()
                .value(),
            Matchers.equalTo(OnDefaultTest.FAKE.value())
        );
    }
}
