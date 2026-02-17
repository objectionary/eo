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
 * Test case for rho attribute handling in {@link PhDefault}.
 * @since 0.1
 */
final class PhDefaultRhoTest {

    @Test
    void doesNotHaveRhoWhenFormed() {
        final Phi phi = new PhSafe(PhDefaultTest.Int.make());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Phi.RHO),
            String.format("Object should not have %s attribute when formed", Phi.RHO)
        );
    }

    @Test
    void setsRhoAfterDispatch() {
        final Phi kid = PhDefaultTest.Int.make().take(this.plus());
        Assertions.assertDoesNotThrow(
            () -> kid.take(Phi.RHO),
            String.format("Kid should have %s attribute after dispatch", Phi.RHO)
        );
    }

    @Test
    void doesNotHaveRhoAfterCopying() {
        final Phi phi = new PhSafe(PhDefaultTest.Int.make().copy());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Phi.RHO),
            String.format("Object should not have %s attribute after copying", Phi.RHO)
        );
    }

    @Test
    void hasKidWithSetRhoAfterCopying() {
        final Phi phi = PhDefaultTest.Int.make().copy();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied kid should be equal to copied parent",
                Phi.RHO
            ),
            phi.take(this.plus()).take(Phi.RHO),
            Matchers.equalTo(phi)
        );
    }

    @Test
    void hasOriginalKidRhoDifferentFromCopy() {
        final Phi phi = PhDefaultTest.Int.make();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of original kid should differ from copy's kid rho", Phi.RHO
            ),
            phi.take(this.plus()).take(Phi.RHO),
            Matchers.not(Matchers.equalTo(phi.copy().take(this.plus()).take(Phi.RHO)))
        );
    }

    @Test
    void hasCopiedKidRhoEqualToCopy() {
        final Phi copy = PhDefaultTest.Int.make().copy();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied kid should refer to copied parent",
                Phi.RHO
            ),
            copy.take(this.plus()).take(Phi.RHO),
            Matchers.equalTo(copy)
        );
    }

    @Test
    void doesNotChangeRhoAfterDirectKidCopying() {
        final Phi plus = PhDefaultTest.Int.make().take(this.plus());
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of kid should not change after direct copying",
                Phi.RHO
            ),
            plus.take(Phi.RHO),
            Matchers.equalTo(plus.copy().take(Phi.RHO))
        );
    }

    @Test
    void doesNotCopyRhoWhileDispatch() {
        final Phi phi = PhDefaultTest.Int.make();
        final Phi plus = phi.take(this.plus());
        MatcherAssert.assertThat(
            String.format("%s attributes should not be copied while dispatch", Phi.RHO),
            plus.take(Phi.RHO),
            Matchers.equalTo(plus.take(Phi.RHO))
        );
    }

    /**
     * Returns the plus attribute name.
     * @return Plus literal
     */
    private String plus() {
        return "plus";
    }
}
