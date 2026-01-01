/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtWithRho}.
 * @since 0.59.0
 */
final class AtWithRhoTest {
    @Test
    void copiesAndSetsRhoIfNotSet() {
        final Phi obj = new PhDefault();
        final Attr attr = new AtComposite(obj, phi -> phi);
        final Phi rho = new PhDefault();
        final Phi phi = new AtWithRho(attr, rho).get();
        MatcherAssert.assertThat(
            "AtWithRho must set RHO if it is not set before",
            phi.take(Phi.RHO),
            Matchers.is(rho)
        );
        MatcherAssert.assertThat(
            "AtWithRho must copy original object if RHO is not set",
            phi,
            Matchers.not(Matchers.is(obj))
        );
    }

    @Test
    void putsObjectToOriginalAttribute() {
        final Attr attr = new AtVoid("void");
        final Phi obj = new PhDefault();
        final Attr rho = new AtWithRho(attr, new PhDefault());
        rho.put(obj);
        MatcherAssert.assertThat(
            "AtWithRho must pass object on put() to original attribute",
            attr.get(),
            Matchers.is(obj)
        );
    }

    @Test
    void doesNotCopyAndSetRhoIfAlreadySet() {
        final Phi obj = new PhDefault();
        final Phi rho = new PhDefault();
        obj.put(Phi.RHO, rho);
        final Phi reset = new PhDefault();
        final Phi res = new AtWithRho(new AtComposite(obj, phi -> phi), reset).get();
        MatcherAssert.assertThat(
            "AtWithRho must not reset RHO if it is already set",
            res.take(Phi.RHO),
            Matchers.is(rho)
        );
        MatcherAssert.assertThat(
            "AtWithRho must not copy original object if RHO is already set",
            res,
            Matchers.is(obj)
        );
    }

    @Test
    void copiesWithNewRho() {
        final Phi obj = new PhDefault();
        final Phi rho = new PhDefault();
        final Phi self = new PhDefault();
        final Phi res = new AtWithRho(new AtComposite(obj, phi -> phi), rho).copy(self).get();
        MatcherAssert.assertThat(
            "AtWithRho must set new RHO on copy() operation",
            res.take(Phi.RHO),
            Matchers.is(self)
        );
        MatcherAssert.assertThat(
            "AtWithRho must call copy() on original object",
            res,
            Matchers.not(Matchers.is(obj))
        );
    }
}
