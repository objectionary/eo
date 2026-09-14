/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Trips}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class TripsTest {

    @Test
    void sumsWhatEveryRunRecorded(@Mktmp final Path temp) throws IOException {
        final Trips trips = new Trips(temp.resolve("deep").resolve("foo.txt"));
        trips.record(17L);
        trips.record(427L);
        MatcherAssert.assertThat(
            "the trips of every run of a document must add up, but they dont",
            trips.total(),
            Matchers.equalTo(444L)
        );
    }

    @Test
    void countsNothingWithoutAFile(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a document that made no run cannot have made a trip, but it did",
            new Trips(temp.resolve("absent.txt")).total(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void forgetsWhatTheEarlierBuildRecorded(@Mktmp final Path temp) throws IOException {
        final Trips trips = new Trips(temp.resolve("foo.txt"));
        trips.record(93L);
        trips.reset();
        MatcherAssert.assertThat(
            "the trips of an earlier build cannot be counted again, but they are",
            trips.total(),
            Matchers.equalTo(0L)
        );
    }
}
