/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
     * Check if commit hashes as map contains given tag as key.
     * @param tag Tag.
     * @checkstyle AnnotationUseStyleCheck (30 lines)
     */
    @ParameterizedTest
    @CsvSource({"0.26.0", "0.28.10"})
    void containsTag(final String tag) {
        MatcherAssert.assertThat(
            String.format(
                "Commit hashes should have contained tag %s, but they didn't",
                tag
            ),
            new CommitHashesMap.Fake(),
            Matchers.hasKey(tag)
        );
    }

    /**
     * Check if commit hashes as map contains valid hash by tag.
     * @param tag Tag.
     * @param hash Hash.
     * @checkstyle AnnotationUseStyleCheck (30 lines)
     */
    @ParameterizedTest
    @CsvSource({
        "0.26.0, e0b7836",
        "0.28.10, 9b88393",
    })
    void containsValidHash(final String tag, final String hash) {
        MatcherAssert.assertThat(
            String.format(
                "Commit hashes should have contained hash %s by tag %s, but they didn't",
                hash,
                tag
            ),
            new CommitHashesMap.Fake().get(tag).value(),
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
