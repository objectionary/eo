/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import org.cactoos.experimental.Threads;
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
        MatcherAssert.assertThat(
            "ChRemote should return the correct commit hash, but it doesn't",
            new ChRemote("0.41.1").value(),
            Matchers.equalTo("9a3dee39597b2e9ac305ca57c296b0fa7e10eb55")
        );
    }

    @Test
    void getsCommitHashOldTag() {
        MatcherAssert.assertThat(
            "ChRemote should return the correct commit hash for an old tag, but it doesn't",
            new ChRemote("0.40.5").value(),
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
