/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
    void decoratesCheckedException() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new AtComposite(
                Phi.Φ,
                self -> {
                    throw new InstantiationException("intended checked");
                }
            ).get(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

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
     * @since 1.0
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
