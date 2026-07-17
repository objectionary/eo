/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Level}.
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class LevelTest {

    @Test
    void retainsIndentFromCtor() {
        MatcherAssert.assertThat(
            "indent must round-trip the ctor argument",
            new Level(4, 7, Kind.HEAD, Openness.OPEN, Kind.BARE_FORMATION, false).indent(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void retainsStartLineFromCtor() {
        MatcherAssert.assertThat(
            "start line must be the line where the level was first pushed",
            new Level(0, 42, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false).start(),
            Matchers.equalTo(42)
        );
    }

    @Test
    void promotesKindOnBecome() {
        final Level level = new Level(
            2, 3, Kind.HEAD, Openness.OPEN, Kind.BARE_FORMATION, false
        );
        level.become(Kind.VAPPLICATION);
        MatcherAssert.assertThat(
            "after become(), kind must reflect the new outer kind",
            level.kind(),
            Matchers.equalTo(Kind.VAPPLICATION)
        );
    }

    @Test
    void downgradesOpennessOnClose() {
        final Level level = new Level(
            0, 1, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.close(Openness.VERTICAL_COMPLETED);
        MatcherAssert.assertThat(
            "after close(), openness must reflect the new state",
            level.openness(),
            Matchers.equalTo(Openness.VERTICAL_COMPLETED)
        );
    }

    @Test
    void flipsNamedFlag() {
        final Level level = new Level(
            0, 1, Kind.BARE_FORMATION, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.name("foo");
        MatcherAssert.assertThat(
            "named() must report true once name() has been called",
            level.named(),
            Matchers.is(true)
        );
    }

    @Test
    void leavesNamedFlagFalseByDefault() {
        MatcherAssert.assertThat(
            "a fresh level cannot be named before name() is invoked",
            new Level(0, 1, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false).named(),
            Matchers.is(false)
        );
    }

    @Test
    void flipsAtomFlag() {
        final Level level = new Level(
            0, 1, Kind.BARE_FORMATION, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.mark();
        MatcherAssert.assertThat(
            "atom() must report true once mark() has been called",
            level.atom(),
            Matchers.is(true)
        );
    }

    @Test
    void recordsParentAtomFromCtor() {
        MatcherAssert.assertThat(
            "parentAtom must round-trip the ctor argument so R-5.3.4 can read it",
            new Level(2, 3, Kind.BARE_FORMATION, Openness.OPEN, Kind.BARE_FORMATION, true)
                .patom(),
            Matchers.is(true)
        );
    }

    @Test
    void consumesReceiverOnce() {
        final Level level = new Level(
            0, 1, Kind.BARE_REVERSED, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.consumeReceiver();
        MatcherAssert.assertThat(
            "receiverConsumed() must flip true after consumeReceiver()",
            level.taken(),
            Matchers.is(true)
        );
    }

    @Test
    void storesCompactNonNegative() {
        final Level level = new Level(
            0, 1, Kind.COMPACT_TUPLE, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.compact(3);
        MatcherAssert.assertThat(
            "compactN() must round-trip the assigned N",
            level.count(),
            Matchers.equalTo(3)
        );
    }

    @Test
    void incrementsChildCount() {
        final Level level = new Level(
            0, 1, Kind.COMPACT_TUPLE, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.child();
        level.child();
        MatcherAssert.assertThat(
            "child() must increment children() by 1 each call",
            level.children(),
            Matchers.equalTo(2)
        );
    }
}
