/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import com.yegor256.WeAreOnline;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link CommitHashesText}.
 *
 * @since 0.37.0
 * @todo #3122:60min Add "Reload" to the test CommitHashesTextTest#isThreadSafe
 *  when issue about "Reload" annotation will be solved.
 *  We need to reinitialize some static fields of the class
 *  before the test will be executed.
 */
final class CommitHashesTextTest {

    @Test
    @ExtendWith(WeAreOnline.class)
    void downloadsDefaultList() throws Exception {
        MatcherAssert.assertThat(
            "CommitHashesText downloads the default list of hashes from Objectionary",
            new CommitHashesText().asString(),
            Matchers.containsString("master")
        );
    }

    @Test
    void isThreadSafe() {
        final int threads = 200;
        boolean nonulls = true;
        for (final boolean bool: new Threads<>(
            threads,
            Stream.generate(
                () -> (Scalar<Boolean>) () -> new CommitHashesText().asString() != null
            ).limit(threads).collect(Collectors.toList())
            )) {
            nonulls &= bool;
        }
        MatcherAssert.assertThat(
            "Can be used in different threads without NPE",
            nonulls,
            Matchers.equalTo(true)
        );
    }
}
