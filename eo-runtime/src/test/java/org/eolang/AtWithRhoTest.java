/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Together;
import java.util.HashSet;
import java.util.concurrent.TimeUnit;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtWithRho}.
 *
 * @since 0.59.0
 */
final class AtWithRhoTest {

    @Test
    void delegatesTermToOriginal() {
        MatcherAssert.assertThat(
            "AtWithRho must delegate φ-term to its original attribute, but it didnt",
            new AtWithRho(new AtVoid("x"), new PhDefault()).φTerm(),
            Matchers.equalTo("?")
        );
    }

    @Test
    void copiesAndSetsRhoIfNotSetMustSetRho() {
        final Phi rho = new PhDefault();
        MatcherAssert.assertThat(
            "AtWithRho must set RHO if it is not set before",
            new AtWithRho(new AtComposite(this.formation(), phi -> phi), rho)
                .get().take(Phi.RHO),
            Matchers.is(rho)
        );
    }

    @Test
    void copiesAndSetsRhoIfNotSetMustCopyOriginal() {
        final Phi obj = this.formation();
        final Phi phi = new AtWithRho(new AtComposite(obj, p -> p), new PhDefault()).get();
        phi.take(Phi.RHO);
        MatcherAssert.assertThat(
            "AtWithRho must copy original object if RHO is not set",
            phi,
            Matchers.not(Matchers.is(obj))
        );
    }

    @Test
    void putsObjectToOriginalAttribute() {
        final Attribute attr = new AtVoid("void");
        final Phi obj = new PhDefault();
        final Attribute rho = new AtWithRho(attr, new PhDefault());
        rho.put(obj);
        MatcherAssert.assertThat(
            "AtWithRho must pass object on put() to original attribute",
            attr.get(),
            Matchers.is(obj)
        );
    }

    @Test
    void doesNotCopyAndSetRhoIfAlreadySetMustNotResetRho() {
        final Phi obj = this.formation();
        final Phi rho = new PhDefault();
        obj.put(Phi.RHO, rho);
        MatcherAssert.assertThat(
            "AtWithRho must not reset RHO if it is already set",
            new AtWithRho(new AtComposite(obj, phi -> phi), new PhDefault()).get().take(Phi.RHO),
            Matchers.is(rho)
        );
    }

    @Test
    void doesNotCopyAndSetRhoIfAlreadySetMustCopyOriginalIfRhoIsSet() {
        final Phi obj = this.formation();
        obj.put(Phi.RHO, new PhDefault());
        final Phi res = new AtWithRho(new AtComposite(obj, phi -> phi), new PhDefault()).get();
        res.take(Phi.RHO);
        MatcherAssert.assertThat(
            "AtWithRho must not copy original object if RHO is already set",
            res,
            Matchers.is(obj)
        );
    }

    @Test
    void copiesWithNewRhoMustSetNewPhoOnCopy() {
        final Phi self = this.formation();
        MatcherAssert.assertThat(
            "AtWithRho must set new RHO on copy() operation",
            new AtWithRho(new AtComposite(this.formation(), phi -> phi), new PhDefault())
                .copy(self).get().take(Phi.RHO),
            Matchers.is(self)
        );
    }

    @Test
    void copiesWithNewRhoMustCallCopyOnOriginal() {
        final Phi obj = this.formation();
        final Phi res = new AtWithRho(new AtComposite(obj, phi -> phi), new PhDefault()).copy(
            new PhDefault()
        ).get();
        res.take(Phi.RHO);
        MatcherAssert.assertThat(
            "AtWithRho must call copy() on original object",
            res,
            Matchers.not(Matchers.is(obj))
        );
    }

    @Test
    void keepsCauseVisibleWhenBindingRhoOntoWrappedTerminator() {
        MatcherAssert.assertThat(
            "AtWithRho must not mask the cause of a wrapped terminator when it binds ρ",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(
                    new AtWithRho(
                        new AtComposite(
                            new PhDefault(),
                            phi -> new PhApplication(
                                new PhTerminator(), 0, new Data.ToPhi("the deep reason")
                            )
                        ),
                        new PhDefault()
                    ).get()
                ).take()
            ).getMessage(),
            Matchers.containsString("the deep reason")
        );
    }

    @Test
    void leavesADeclaredReceiverUnwrapped() {
        final PhDefault host = new PhDefault();
        host.add("kid", new AtComposite(host, rho -> this.formation()));
        MatcherAssert.assertThat(
            "a declared receiver must hand out the host itself, but it didnt",
            host.take("kid").take(Phi.RHO),
            Matchers.is(host)
        );
    }

    @RepeatedTest(20)
    void handsOutItsOwnCopyToEveryConcurrentCaller() {
        final int callers = 16;
        final Attribute attr = new AtWithRho(
            new AtOnce(new AtComposite(new PhDefault(), phi -> this.formation())),
            new PhDefault()
        );
        MatcherAssert.assertThat(
            "every concurrent call must take a copy of its own, but some of them shared one",
            new HashSet<>(
                new Together<>(callers, thread -> attr.get())
                    .withTimeout(1L, TimeUnit.MINUTES)
                    .asList()
            ),
            Matchers.hasSize(callers)
        );
    }

    @RepeatedTest(20)
    void bindsOneReceiverOntoEveryConcurrentCopy() {
        final Phi rho = new PhDefault();
        final Attribute attr = new AtWithRho(
            new AtOnce(new AtComposite(new PhDefault(), phi -> this.formation())),
            rho
        );
        MatcherAssert.assertThat(
            "every concurrent copy must carry the very same receiver, but some carried another",
            new HashSet<>(
                new Together<>(16, thread -> attr.get().take(Phi.RHO))
                    .withTimeout(1L, TimeUnit.MINUTES)
                    .asList()
            ),
            Matchers.contains(rho)
        );
    }

    @Test
    void handsOutItsOwnCopyOnEveryCall() {
        final Attribute attr = new AtWithRho(
            new AtOnce(new AtComposite(new PhDefault(), phi -> this.formation())),
            new PhDefault()
        );
        MatcherAssert.assertThat(
            "the second call took the copy the first one took, but each call must take its own",
            attr.get(),
            Matchers.not(Matchers.sameInstance(attr.get()))
        );
    }

    private Phi formation() {
        return new PhDefault(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }
}
