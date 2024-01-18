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
 * Test case for {@link PhConst}.
 *
 * @since 0.16
 */
final class PhConstTest {

    @Test
    void makesObjectConstant() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhConst(
                    new Data.ToPhi("Hello, world!")
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void caclulatesPhiOnlyOnce() {
        final Dummy dummy = new Dummy("any");
        final Phi phi = new PhConst(dummy);
        for (int idx = 0; idx < 10; ++idx) {
            MatcherAssert.assertThat(
                new Dataized(phi).take(Long.class),
                Matchers.is(1L)
            );
        }
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void dataizesOnceEvenIfCopied() {
        final Dummy dummy = new Dummy("child");
        final Phi child = new PhMethod(new PhConst(dummy), "child");
        new Dataized(child).take(Long.class);
        final Phi copy = new PhWith(new PhCopy(child), 0, new Data.ToPhi(1L));
        new Dataized(copy).take(Long.class);
        MatcherAssert.assertThat(dummy.count, Matchers.equalTo(1));
    }

    @Test
    void dataizesSimpleRandomToConst() {
        final Phi rnd = new PhConstTest.Rnd(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(rnd).take(Double.class),
            Matchers.not(
                Matchers.equalTo(new Dataized(rnd).take(Double.class))
            )
        );
        final Phi cnst = new PhConst(rnd);
        MatcherAssert.assertThat(
            new Dataized(cnst).take(Double.class),
            Matchers.equalTo(new Dataized(cnst).take(Double.class))
        );
    }

    @Test
    void dataizesNegRandomToConst() {
        final Phi cnst = new PhConst(new PhConstTest.Rnd(Phi.Φ));
        final double first = new Dataized(
            cnst.attr("neg").get()
        ).take(Double.class);
        final double second = new Dataized(
            cnst.attr("neg").get()
        ).take(Double.class);
        MatcherAssert.assertThat(first, Matchers.equalTo(second));
    }

    @Test
    void dataizesRandomToConst() {
        final Phi rnd = new PhConst(new PhConstTest.Rnd(Phi.Φ));
        final Phi eql = rnd.attr("eq").get();
        eql.attr(0).put(rnd);
        MatcherAssert.assertThat(
            new Dataized(eql).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    void doesNotAllowAttributesOfDecorateeToBeSet() {
        final Phi phi = new Boom();
        Assertions.assertThrows(
            ExUnset.class,
            () -> phi.attr("x").put(new Data.ToPhi(1L))
        );
    }

    @Test
    void makesRhoConstToo() {
        final String name = "kid";
        final Dummy dummy = new Dummy(name);
        final Phi mtd = new PhMethod(new PhConst(dummy), name);
        for (int idx = 0; idx < 10; ++idx) {
            MatcherAssert.assertThat(
                new Dataized(mtd).take(Long.class),
                Matchers.is(1L)
            );
        }
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void keepsDecorateeConst() {
        final Boom boom = new Boom();
        final Phi cnst = new PhConst(boom);
        for (int idx = 0; idx < 10; ++idx) {
            final Phi phi = cnst.attr("φ").get().copy();
            phi.attr("x").put(new Data.ToPhi(1L));
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(boom.count, Matchers.equalTo(1));
    }

    @Test
    void keepsConstMultiLayers() {
        final Phi phi = new PhWith(
            new Envelope(Phi.Φ),
            0,
            new PhWith(
                new Envelope(Phi.Φ),
                0,
                new PhConst(new Rnd(Phi.Φ))
            )
        );
        MatcherAssert.assertThat(
            new Dataized(phi).take(Double.class),
            Matchers.equalTo(new Dataized(phi).take(Double.class))
        );
    }

    @Test
    void dataizesOnlyOnceViaEnvelopes() {
        final Dummy dummy = new Dummy("x");
        final Phi phi = new PhConst(
            new PhWith(
                new Envelope(Phi.Φ),
                0,
                new PhWith(
                    new Envelope(Phi.Φ),
                    0,
                    new PhWith(
                        new Envelope(Phi.Φ),
                        0, dummy
                    )
                )
            )
        );
        phi.attr("eq").get();
        new Dataized(phi).take(Long.class);
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void dataizesOnlyOnceViaMethods() {
        final Dummy dummy = new Dummy("x");
        final Phi phi = new PhConst(
            new PhWith(
                new Envelope(Phi.Φ),
                0,
                new PhWith(
                    new Envelope(Phi.Φ),
                    0,
                    new PhWith(
                        new Envelope(Phi.Φ),
                        0, dummy
                    )
                )
            )
        );
        new Dataized(phi).take();
        new Dataized(phi).take();
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    /**
     * Rnd.
     * @since 1.0
     */
    private static class Rnd extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Rnd(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    rho -> new Data.ToPhi(new SecureRandom().nextDouble())
                )
            );
        }
    }

    /**
     * Dummy Phi.
     * @since 1.0
     */
    private static class Dummy extends PhDefault {
        /**
         * Count.
         */
        private int count;

        /**
         * Ctor.
         * @param name Name
         */
        Dummy(final String name) {
            super();
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        ++this.count;
                        return new Data.ToPhi(1L);
                    }
                )
            );
            this.add(name, new AtComposite(this, PhConstTest.Kid::new));
        }
    }

    /**
     * Kid Phi.
     * @since 1.0
     */
    private static class Kid extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Kid(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new Data.ToPhi(
                        new Dataized(self.attr("ρ").get()).take(Long.class)
                    )
                )
            );
        }
    }

    /**
     * Phi envelope.
     * @since 1.0
     */
    private static class Envelope extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Envelope(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
            this.add("φ", new AtOnce(new AtComposite(this, rho -> rho.attr("x").get())));
        }
    }

    /**
     * Boom Phi.
     * @since 1.0
     */
    private static class Boom extends PhDefault {
        /**
         * Count.
         */
        private int count;

        /**
         * Ctor.
         */
        Boom() {
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        ++this.count;
                        return new Sub(self);
                    }
                )
            );
        }
    }

    /**
     * Sub phi.
     * @since 1.0
     */
    private static class Sub extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Sub(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
            this.add("φ", new AtComposite(this, self -> new Data.ToPhi(1L)));
        }
    }
}
