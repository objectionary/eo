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
package org.eolang.maven.util;

import org.eolang.maven.hash.CommitHash;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link ObjectFullName}.
 *
 * @since 0.29.6
 */
final class ObjectFullNameTest {
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
        ObjectFullNameTest.STDOUT,
        ObjectFullNameTest.HASH
    );

    /**
     * Fake commit hash.
     */
    private static final CommitHash FAKE = new CommitHash.ChConstant("abcdefg");

    @Test
    void returnsTheSameValidFullName() {
        MatcherAssert.assertThat(
            new ObjectFullName(ObjectFullNameTest.OBJECT, ObjectFullNameTest.FAKE).asString(),
            Matchers.equalTo(ObjectFullNameTest.OBJECT)
        );
    }

    @Test
    void returnsFullNameWithoutVersion() {
        MatcherAssert.assertThat(
            new ObjectFullName(
                ObjectFullNameTest.OBJECT,
                ObjectFullNameTest.FAKE,
                false
            ).asString(),
            Matchers.equalTo(ObjectFullNameTest.OBJECT)
        );
    }

    @Test
    void takesNameFromGivenFullName() {
        MatcherAssert.assertThat(
            new ObjectFullName(
                ObjectFullNameTest.OBJECT,
                ObjectFullNameTest.FAKE
            ).name(),
            Matchers.equalTo(ObjectFullNameTest.STDOUT)
        );
    }

    @Test
    void takesHashFromGivenFullName() {
        MatcherAssert.assertThat(
            new ObjectFullName(
                ObjectFullNameTest.OBJECT,
                ObjectFullNameTest.FAKE
            )
                .hash()
                .value(),
            Matchers.equalTo(ObjectFullNameTest.HASH)
        );
    }

    @Test
    void buildsFullNameWithGivenDefaultHash() {
        MatcherAssert.assertThat(
            new ObjectFullName(
                ObjectFullNameTest.STDOUT,
                ObjectFullNameTest.FAKE
            ).asString(),
            Matchers.equalTo(
                String.join("|", ObjectFullNameTest.STDOUT, ObjectFullNameTest.FAKE.value())
            )
        );
    }

    @Test
    void takesHashFromGivenDefaultHash() {
        MatcherAssert.assertThat(
            new ObjectFullName(
                ObjectFullNameTest.STDOUT,
                ObjectFullNameTest.FAKE
            )
                .hash()
                .value(),
            Matchers.equalTo(ObjectFullNameTest.FAKE.value())
        );
    }
}
