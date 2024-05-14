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
import java.util.Optional;
import java.util.function.Supplier;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtComposite}.
 *
 * @since 0.16
 */
public final class AtCompositeTest {

    /**
     * Supplier for failed assert messages.
     *
     * {@link AtCompositeTest#FAILED_ASSERT_MESSAGE_SUPPLIER} field in
     *  eo-runtime for getting failed assert messages with class and test
     */
    public static final Supplier<String> FAILED_ASSERT_MESSAGE_SUPPLIER =
            () -> "Failed " + Thread.currentThread().getStackTrace()[2].getClassName()
                    + '.'
                    + Thread.currentThread().getStackTrace()[2].getMethodName();

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
            FAILED_ASSERT_MESSAGE_SUPPLIER.get()
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
            FAILED_ASSERT_MESSAGE_SUPPLIER.get()
        );
    }

    @Test
    void goesThroughJustOnce() {
        final Phi rnd = new Rnd();
        final Phi phi = new PhMethod(rnd, Attr.LAMBDA);
        MatcherAssert.assertThat(
            FAILED_ASSERT_MESSAGE_SUPPLIER.get(),
            new Dataized(phi).take(Double.class),
            Matchers.equalTo(
                new Dataized(phi).take(Double.class)
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
