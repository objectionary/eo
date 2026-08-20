/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Test case for {@link Kind}.
 * @since 0.1
 */
final class KindTest {

    @ParameterizedTest
    @EnumSource(
        value = Kind.class,
        names = {
            "HAPPLICATION",
            "REVERSED_HARGS",
            "VMETHOD_HARGS"
        }
    )
    void marksHorizontallyCompletedKinds(final Kind kind) {
        MatcherAssert.assertThat(
            "Appendix A's horizontally-completed set must report true",
            kind.horizontallyCompleted(),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @EnumSource(
        value = Kind.class,
        names = {
            "TOP_LEVEL", "HEAD", "HMETHOD", "BARE_FORMATION", "BARE_REVERSED",
            "COMPACT_TUPLE", "ONLY_PHI", "VAPPLICATION", "VMETHOD",
            "TEXT_BLOCK", "IDENTITY_OBJECT"
        }
    )
    void leavesOtherKindsOutOfHorizontallyCompletedSet(final Kind kind) {
        MatcherAssert.assertThat(
            "kinds outside Appendix A's horizontally-completed set must report false",
            kind.horizontallyCompleted(),
            Matchers.is(false)
        );
    }

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

    @Test
    void keepsIdentityObjectOutOfFormationSet() {
        MatcherAssert.assertThat(
            "an identity object binds no body, so its children stay arguments rather than bindings",
            Kind.IDENTITY_OBJECT.formation(),
            Matchers.is(false)
        );
    }
}
