/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
public final class AtCompositeTest {

    @Test
    public void decoratesCheckedException() {
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
    public void decoratesUncheckedException() {
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
    public void passesSelfCorrectly() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhConst(dummy);
        phi.attr("φ").get();
        MatcherAssert.assertThat(
            dummy.self,
            Matchers.equalTo(phi)
        );
    }

    @Test
    public void passesSelfCorrectlyThroughChild() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhConst(dummy);
        phi.attr("φ").get();
        MatcherAssert.assertThat(
            dummy.self,
            Matchers.equalTo(phi)
        );
    }

    @Test
    public void goesThroughJustOnce() {
        final Phi rnd = new Rnd();
        final Phi phi = new PhMethod(rnd, "φ");
        MatcherAssert.assertThat(
            new Dataized(phi).take(Double.class),
            Matchers.equalTo(
                new Dataized(phi).take(Double.class)
            )
        );
    }

    private static class Dummy extends PhDefault {
        public Phi self;
        Dummy() {
            super();
            this.add("φ", new AtComposite(this, rho -> {
                this.self = rho;
                return new Data.ToPhi(1L);
            }));
        }
    }

    private static class Rnd extends PhDefault {
        Rnd() {
            super();
            this.add("φ", new AtComposite(this,
                rho -> new Data.ToPhi(new SecureRandom().nextDouble())
            ));
        }
    }
}
