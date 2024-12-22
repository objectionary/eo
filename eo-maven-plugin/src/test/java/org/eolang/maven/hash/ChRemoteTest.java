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

import com.yegor256.WeAreOnline;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import org.cactoos.experimental.Threads;
import org.eolang.maven.CatalogsTest;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link ChRemote}.
 * @since 0.26
 */
@ExtendWith(WeAreOnline.class)
final class ChRemoteTest {

    @Test
    void getsCommitHashTag() {
        final String hash = new ChRemote("0.41.1").value();
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            hash,
            Matchers.equalTo("9a3dee39597b2e9ac305ca57c296b0fa7e10eb55")
        );
    }

    @Test
    void getsCommitHashOldTag() {
        final String hash = new ChRemote("0.40.5").value();
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            hash,
            Matchers.equalTo("ee14a1e30a9e0f8f64404e7a52f40c2a07f88359")
        );
    }

    @Test
    void throwsCommitHashException() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new ChRemote("nonsense").value(),
            "Throws when tag is not found"
        );
    }

    @Test
    void isThreadSafe() {
        final int threads = 200;
        final String sample = new ChRemote("0.40.5").value();
        MatcherAssert.assertThat(
            "You can use this class concurrently",
            StreamSupport.stream(
                new Threads<>(
                    threads,
                    Stream.generate(
                        () -> new ChRemote("0.40.5")
                    ).limit(threads).collect(Collectors.toList())
                ).spliterator(), false
            ).filter(str -> !sample.equals(str)).collect(Collectors.toList()),
            Matchers.empty()
        );
    }
}
