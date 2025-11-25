/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhComposite}.
 *
 * @since 0.16
 */
@SuppressWarnings("PMD.JUnit5TestShouldBePackagePrivate")
final class PhCompositeTest {

    @Test
    void decoratesUncheckedException() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new PhComposite(
                Phi.Φ,
                self -> {
                    throw new IllegalStateException("intended unchecked");
                }
            ).take(0),
            "PhComposite must decorate unchecked exception correctly, but it didn't"
        );
    }

    @Test
    void goesThroughJustOnce() {
        final Phi rnd = new Rnd();
        final Phi phi = new PhMethod(rnd, Phi.LAMBDA);
        MatcherAssert.assertThat(
            "Generated phi should be same on second access, but it didn't",
            new Dataized(phi).asNumber(),
            Matchers.equalTo(
                new Dataized(phi).asNumber()
            )
        );
    }

    /**
     * Rnd.
     * @since 0.1.0
     */
    private static class Rnd extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Rnd() {
            super();
            this.add(
                Phi.LAMBDA,
                new AtComposite(
                    this,
                    rho -> new Data.ToPhi(new SecureRandom().nextDouble())
                )
            );
        }
    }
}
