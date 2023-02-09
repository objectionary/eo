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

import java.io.IOException;
import java.util.Collections;
import org.cactoos.text.TextOf;
import org.eolang.maven.OnlineCondition;
import org.eolang.maven.OyFake;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link OyIndexed}.
 *
 * @since 0.29
 */
class OyIndexedTest {

    /**
     * Object name for stdout.
     */
    private static final String STDOUT_OBJECT = "org.eolang.io.stdout";

    @Test
    void getsFromDelegate() throws Exception {
        MatcherAssert.assertThat(
            new TextOf(new OyIndexed(new OyFake()).get("foo")).asString(),
            Matchers.equalTo("[] > sprintf\n")
        );
    }

    @Test
    @ExtendWith(OnlineCondition.class)
    void containsInRealIndex() throws IOException {
        MatcherAssert.assertThat(
            new OyIndexed(new OyFake()).contains(OyIndexedTest.STDOUT_OBJECT),
            Matchers.is(true)
        );
    }

    @Test
    void containsInFakeIndex() throws IOException {
        MatcherAssert.assertThat(
            new OyIndexed(
                new OyFake(),
                new ObjectsIndex(() -> Collections.singleton(OyIndexedTest.STDOUT_OBJECT))
            ).contains(OyIndexedTest.STDOUT_OBJECT),
            Matchers.is(true)
        );
    }

    @Test
    void checksContainsInDelegateIfExceptionHappensInIndex() throws IOException {
        MatcherAssert.assertThat(
            new OyIndexed(
                new OyFake(),
                new ObjectsIndex(
                    () -> {
                        throw new IllegalStateException("Fake exception");
                    }
                )
            ).contains(OyIndexedTest.STDOUT_OBJECT),
            Matchers.is(true)
        );
    }
}
