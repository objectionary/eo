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
final class AtCompositeTest {

    @Test
    void decoratesCheckedException() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new AtComposite(
                Phi.Φ,
                self -> {
                    throw new InstantiationException("intended checked");
                }
            ).get()
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
            ).get()
        );
    }

    @Test
    void passesSelfCorrectly() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhConst(dummy);
        phi.attr(Attr.LAMBDA).get();
        MatcherAssert.assertThat(
            dummy.self,
            Matchers.equalTo(phi)
        );
    }

    @Test
    void passesSelfCorrectlyThroughChild() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhConst(dummy);
        phi.attr(Attr.LAMBDA).get();
        MatcherAssert.assertThat(
            dummy.self,
            Matchers.equalTo(phi)
        );
    }

    @Test
    void goesThroughJustOnce() {
        final Phi rnd = new Rnd();
        final Phi phi = new PhMethod(rnd, Attr.LAMBDA);
        MatcherAssert.assertThat(
            new Dataized(phi).take(Double.class),
            Matchers.equalTo(
                new Dataized(phi).take(Double.class)
            )
        );
    }

    /**
     * Dummy phi.
     * @since 1.0
     */
    private static class Dummy extends PhDefault {
        /**
         * Self.
         */
        private Phi self;

        /**
         * Ctor.
         */
        Dummy() {
            super();
            this.add(
                Attr.LAMBDA,
                new AtComposite(
                    this,
                    rho -> {
                        this.self = rho;
                        return new Data.ToPhi(1L);
                    }
                )
            );
        }
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
