/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Test case for {@link Kind}.
 *
 * @since 0.1
 */
final class KindTest {

    @ParameterizedTest
    @EnumSource(
        value = Kind.class,
        names = {"BARE_FORMATION", "ONLY_PHI", "PIPE_APPLICATION", "IDENTITY_OBJECT"}
    )
    void acceptsPipeAfterFormationLikeKinds(final Kind kind) {
        MatcherAssert.assertThat(
            "a pipe must attach to every formation-like kind, the identity object included",
            kind.pipeable(),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @EnumSource(
        value = Kind.class,
        names = {"BARE_FORMATION", "ONLY_PHI", "PIPE_APPLICATION", "IDENTITY_OBJECT"},
        mode = EnumSource.Mode.EXCLUDE
    )
    void rejectsPipeAfterOtherKinds(final Kind kind) {
        MatcherAssert.assertThat(
            "a pipe cannot attach to a kind that is neither formation-like nor a pipe",
            kind.pipeable(),
            Matchers.is(false)
        );
    }

    @ParameterizedTest
    @EnumSource(
        value = Kind.class,
        names = {"BARE_FORMATION", "ONLY_PHI"}
    )
    void marksFormationKinds(final Kind kind) {
        MatcherAssert.assertThat(
            "a formation kind must open a fresh naming scope",
            kind.formation(),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @EnumSource(
        value = Kind.class,
        names = {"BARE_FORMATION", "ONLY_PHI"},
        mode = EnumSource.Mode.EXCLUDE
    )
    void leavesOtherKindsOutOfFormations(final Kind kind) {
        MatcherAssert.assertThat(
            "a kind that opens no naming scope must not report itself as a formation",
            kind.formation(),
            Matchers.is(false)
        );
    }
}
