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
 * Test case for {@link AtRho}.
 * @since 0.59.0
 */
final class AtRhoTest {
    @Test
    void throwsOnEmptyRho() {
        Assertions.assertThrows(
            ExUnset.class,
            new AtRho()::get,
            "AtRho must throw an exception if attribute is not set"
        );
    }

    @Test
    void returnsSetRho() {
        final Attr rho = new AtRho();
        final Phi obj = new PhDefault();
        rho.put(obj);
        MatcherAssert.assertThat(
            "AtRho must successfully return set object",
            rho.get(),
            Matchers.is(obj)
        );
    }

    @Test
    void doesNotCopyObjectOnCopying() {
        final Attr rho = new AtRho();
        final Phi obj = new PhDefault();
        rho.put(obj);
        MatcherAssert.assertThat(
            "AtRho must not copy inner object on copy() operation",
            rho.copy(new PhDefault()).get(),
            Matchers.is(obj)
        );
    }

    @Test
    void doesNotResetObject() {
        final Attr rho = new AtRho();
        final Phi obj = new PhDefault();
        rho.put(obj);
        rho.put(new PhDefault());
        MatcherAssert.assertThat(
            "AtRho must not change state after put()",
            rho.get(),
            Matchers.equalTo(obj)
        );
    }
}
