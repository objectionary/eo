/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Level}.
 * @since 0.1
 */
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
        level.close(Openness.VCOMPLETED);
        MatcherAssert.assertThat(
            "after close(), openness must reflect the new state",
            level.openness(),
            Matchers.equalTo(Openness.VCOMPLETED)
        );
    }

    @Test
    void flipsNamedFlag() {
        final Level level = new Level(
            0, 1, Kind.BARE_FORMATION, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.name("foo", false);
        MatcherAssert.assertThat(
            "named() must report true once name() has been called",
            level.named(),
            Matchers.is(true)
        );
    }

    @Test
    void dropsNamedFlagOnSeal() {
        final Level level = new Level(
            2, 5, Kind.VMETHOD, Openness.OPEN, Kind.BARE_FORMATION, false
        );
        level.name("intermediate", false);
        level.sealed();
        MatcherAssert.assertThat(
            "sealed() must forget the name the replaced chain link carried",
            level.named(),
            Matchers.is(false)
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
            "patom() must round-trip the ctor argument so R-5.3.4 can read it",
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
            "taken() must flip true after consumeReceiver()",
            level.taken(),
            Matchers.is(true)
        );
    }

    @Test
    void retainsCompactCountFromCompact() {
        final Level level = new Level(
            0, 1, Kind.COMPACT_TUPLE, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.compact(3);
        MatcherAssert.assertThat(
            "count() must round-trip the N passed to compact()",
            level.count(),
            Matchers.equalTo(3)
        );
    }

    @Test
    void positionsMismatchedBindingErrorAtObservedSpan() {
        final Level level = new Level(
            0, 1, Kind.COMPACT_TUPLE, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.observeBinding(false, new Span("first", 1));
        level.observeBinding(true, new Span("second", 9));
        MatcherAssert.assertThat(
            "error must be positioned at the line of the arg that broke the rule",
            Assertions.assertThrows(
                ParseError.class,
                level::commitArg,
                "commitArg must reject a binding that flips mode mid-group"
            ).line(),
            Matchers.equalTo(9)
        );
    }

    @Test
    void toleratesCommitArgWithoutPendingArg() {
        Assertions.assertDoesNotThrow(
            new Level(
                0, 1, Kind.COMPACT_TUPLE, Openness.OPEN, Kind.TOP_LEVEL, false
            )::commitArg,
            "commitArg must not raise when no arg is currently pending"
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

    @Test
    void acceptsVoidBeforeAnyPlainChild() {
        final Level level = new Level(
            0, 1, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        Assertions.assertDoesNotThrow(
            () -> level.observeVoid(Kind.VOID, 2, 3),
            "observeVoid must not reject a void child that has no plain sibling yet"
        );
    }

    @Test
    void acceptsPlainChildAfterVoid() {
        final Level level = new Level(
            0, 1, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.observeVoid(Kind.VOID, 2, 3);
        Assertions.assertDoesNotThrow(
            () -> level.observeVoid(Kind.HEAD, 4, 5),
            "observeVoid must not reject a plain child that follows a void one"
        );
    }

    @Test
    void rejectsVoidAfterPlainChild() {
        final Level level = new Level(
            0, 1, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.observeVoid(Kind.HEAD, 2, 3);
        Assertions.assertThrows(
            ParseError.class,
            () -> level.observeVoid(Kind.VOID, 4, 5),
            "observeVoid must reject a void child once a plain sibling has appeared"
        );
    }

    @Test
    void positionsVoidOrderingErrorAtOffendingVoid() {
        final Level level = new Level(
            0, 1, Kind.HEAD, Openness.OPEN, Kind.TOP_LEVEL, false
        );
        level.observeVoid(Kind.HEAD, 2, 3);
        MatcherAssert.assertThat(
            "error must be positioned at the line of the void that broke the ordering rule",
            Assertions.assertThrows(
                ParseError.class,
                () -> level.observeVoid(Kind.VOID, 9, 4),
                "observeVoid must reject a misplaced void with a ParseError"
            ).line(),
            Matchers.equalTo(9)
        );
    }
}
