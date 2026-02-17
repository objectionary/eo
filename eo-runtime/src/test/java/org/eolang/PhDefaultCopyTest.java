/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for copy behavior in {@link PhDefault}.
 * @since 0.1
 */
final class PhDefaultCopyTest {

    @Test
    void comparesSelfToCopy() {
        final Phi phi = PhDefaultTest.Int.make();
        MatcherAssert.assertThat(
            "Object should not be equal to its copy",
            phi, Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void comparesTwoCopies() {
        final Phi phi = PhDefaultTest.Int.make();
        MatcherAssert.assertThat(
            "Two copies of object should not be equal to each other",
            phi.copy(), Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void copiesKid() {
        final Phi phi = PhDefaultTest.Int.make();
        MatcherAssert.assertThat(
            "Child attributes should be copied after copying main object",
            phi.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(phi.copy().take(this.plus()))
            )
        );
    }

    @Test
    void takesDifferentAbstractKidsEveryDispatch() {
        final Phi phi = PhDefaultTest.Int.make();
        MatcherAssert.assertThat(
            "Child attributes should be copied on every dispatch",
            phi.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(phi.take(this.plus()))
            )
        );
    }

    @Test
    void hasDifferentKidsAfterDoubleCopying() {
        final Phi first = PhDefaultTest.Int.make().copy();
        MatcherAssert.assertThat(
            "Child objects after double copying should be different",
            first.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(first.copy().take(this.plus()))
            )
        );
    }

    @Test
    void copiesUnsetVoidAttribute() {
        final Phi phi = new PhSafe(PhDefaultTest.Int.make());
        final Phi copy = phi.copy();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> copy.take(this.getVoid()),
            "Unset void attribute should be copied with unset value"
        );
    }

    /**
     * Returns the plus attribute name.
     * @return Plus literal
     */
    private String plus() {
        return "plus";
    }

    /**
     * Returns the void attribute name.
     * @return Void literal
     */
    private String getVoid() {
        return "void";
    }
}
