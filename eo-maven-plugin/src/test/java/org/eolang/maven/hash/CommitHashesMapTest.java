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

import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link CommitHashesMap}.
 *
 * @since 0.29.5
 */
final class CommitHashesMapTest {

    /**
     * Check if commit hashes as map contains given tag as key and hash by tag.
     * @param tag Tag.
     * @param hash Hash.
     * @checkstyle AnnotationUseStyleCheck (40 lines)
     */
    @ParameterizedTest
    @CsvSource({
        "0.26.0, e0b7836",
        "0.28.10, 9b88393",
    })
    void containsValidHash(final String tag, final String hash) {
        final Map<String, CommitHash> hashes = new CommitHashesMap();
        MatcherAssert.assertThat(
            String.format(
                "Commit hashes should have contained tag %s, but they didn't",
                tag
            ),
            hashes,
            Matchers.hasKey(tag)
        );
        MatcherAssert.assertThat(
            String.format(
                "Commit hashes should have contained hash %s by tag %s, but they didn't",
                hash,
                tag
            ),
            hashes.get(tag).value(),
            Matchers.equalTo(hash)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "0.26.1",
        "100.100.100",
    })
    void doesNotContainTag(final String tag) {
        MatcherAssert.assertThat(
            String.format(
                "Commit hashes should have contained tag %s, but they didn't",
                tag
            ),
            new CommitHashesMap(),
            Matchers.not(
                Matchers.hasKey(tag)
            )
        );
    }
}
