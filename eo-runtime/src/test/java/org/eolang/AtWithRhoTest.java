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
    void setsRhoIfNotSet() {
        final Phi rho = new PhDefault();
        MatcherAssert.assertThat(
            "AtWithRho must set RHO if it is not set before",
            new AtWithRho(new AtComposite(new PhDefault(), p -> p), rho).get().take(Phi.RHO),
            Matchers.is(rho)
        );
    }

    @Test
    void copiesIfRhoNotSet() {
        final Phi obj = new PhDefault();
        MatcherAssert.assertThat(
            "AtWithRho must copy original object if RHO is not set",
            new AtWithRho(new AtComposite(obj, p -> p), new PhDefault()).get(),
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
    void doesNotResetRhoIfSet() {
        final Phi obj = new PhDefault();
        final Phi rho = new PhDefault();
        obj.put(Phi.RHO, rho);
        MatcherAssert.assertThat(
            "AtWithRho must not reset RHO if it is already set",
            new AtWithRho(new AtComposite(obj, p -> p), new PhDefault()).get().take(Phi.RHO),
            Matchers.is(rho)
        );
    }

    @Test
    void doesNotCopyIfRhoSet() {
        final Phi obj = new PhDefault();
        obj.put(Phi.RHO, new PhDefault());
        MatcherAssert.assertThat(
            "AtWithRho must not copy original object if RHO is already set",
            new AtWithRho(new AtComposite(obj, p -> p), new PhDefault()).get(),
            Matchers.is(obj)
        );
    }

    @Test
    void setsNewRhoOnCopy() {
        final Phi self = new PhDefault();
        MatcherAssert.assertThat(
            "AtWithRho must set new RHO on copy() operation",
            new AtWithRho(new AtComposite(new PhDefault(), p -> p), new PhDefault())
                .copy(self).get().take(Phi.RHO),
            Matchers.is(self)
        );
    }

    @Test
    void copiesOriginalOnCopy() {
        final Phi obj = new PhDefault();
        MatcherAssert.assertThat(
            "AtWithRho must call copy() on original object",
            new AtWithRho(new AtComposite(obj, p -> p), new PhDefault())
                .copy(new PhDefault()).get(),
            Matchers.not(Matchers.is(obj))
        );
    }
}
