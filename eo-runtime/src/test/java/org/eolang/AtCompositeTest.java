/*
 * The MIT License (MIT)
 *
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
 * Test case for {@link AtComposite}.
 *
 * @since 0.16
 */
@SuppressWarnings("PMD.JUnit5TestShouldBePackagePrivate")
public final class AtCompositeTest {

    /**
     * Empty message for JUnit Assertions.
     *
     * @todo #2297:60m Replace all appearances of {@link AtCompositeTest#TO_ADD_MESSAGE} field in
     *  eo-runtime with meaningful assert messages. Don't forget to remove
     *  {@link AtCompositeTest#TO_ADD_MESSAGE} field and remove public modifier from this class if
     *  no longer need.
     */
    public static final String TO_ADD_MESSAGE = "TO ADD ASSERTION MESSAGE";

    @Test
    void decoratesUncheckedException() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new AtComposite(
                Phi.Φ,
                self -> {
                    throw new IllegalStateException("intended unchecked");
                }
            ).get(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void goesThroughJustOnce() {
        final Phi rnd = new Rnd();
        final Phi phi = new PhMethod(rnd, Attr.LAMBDA);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
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
                Attr.LAMBDA,
                new AtComposite(
                    this,
                    rho -> new Data.ToPhi(new SecureRandom().nextDouble())
                )
            );
        }
    }
}
