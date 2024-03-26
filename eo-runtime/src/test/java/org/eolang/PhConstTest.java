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
import net.sf.saxon.expr.parser.Loc;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test case for @link PhConst}.
 * @since 0.16
 */
final class PhConstTest {
    @Test
    void makesDataObjectConstant() {
        MatcherAssert.assertThat(
            "Const data object should be dataizable",
            new Dataized(
                new PhConst(
                    new Data.ToPhi("Hello, world!")
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    void behavesAsBytes() {
        MatcherAssert.assertThat(
            "Const object should have 'as-int' attribute which is from 'bytes' object",
            new Dataized(new PhConst(new Data.ToPhi(42L)).attr("as-int").get()).take(Long.class),
            Matchers.equalTo(42L)
        );
    }

    @Test
    void caclulatesPhiOnlyOnce() {
        final Dummy dummy = new PhConstTest.Dummy("any");
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
    void doesNotHaveAttributesOfDecoratedPhi() {
        final Phi phi = new PhConst(new PhConstTest.Dummy("attr"));
        Assertions.assertThrows(
            ExUnset.class,
            () -> phi.attr("attr").get(),
            "Const object should behave as bytes, not as decorated object"
        );
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
    void dataizesRandomToConst() {
        final Phi rnd = new PhConst(new PhConstTest.Rnd(Phi.Φ));
        final Phi eql = rnd.attr("eq").get().copy();
        eql.attr(0).put(rnd);
        MatcherAssert.assertThat(
            new Dataized(eql).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    void doesNotRecalculateAfterCopying() {
        final Phi rnd = new PhConst(new PhConstTest.Rnd(Phi.Φ));
        MatcherAssert.assertThat(
            "Const object should not be recalculated after copying",
            new Dataized(rnd).take(Long.class),
            Matchers.equalTo(
                new Dataized(rnd.copy()).take(Long.class)
            )
        );
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
        final Phi phi = new PhConst(new EnvelopedDummy(Phi.Φ, dummy));
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
        final Phi phi = new PhConst(new EnvelopedDummy(Phi.Φ, dummy));
        new Dataized(phi).take();
        new Dataized(phi).take();
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void usesStringRepresentationOfWrapped() {
        final Phi phi = new PhConstTest.Dummy("some");
        MatcherAssert.assertThat(
            "Const object should use string representation of wrapped object",
            new PhConst(phi).toString(),
            Matchers.equalTo(String.format("!%s", phi))
        );
    }

    @Test
    void usesTermRepresentationOfWrapped() {
        final Phi phi = new PhConstTest.Dummy("some");
        MatcherAssert.assertThat(
            "Const object should use term representation of wrapped object",
            new PhConst(phi).φTerm(),
            Matchers.equalTo(String.format("%s!", phi.φTerm()))
        );
    }

    @Test
    void usesLocatorOfWrapped() {
        final Phi phi = new PhConstTest.Dummy("some");
        MatcherAssert.assertThat(
            "Const object should use locator of wrapped object",
            new PhConst(phi).locator(),
            Matchers.equalTo(phi.locator())
        );
    }

    @Test
    void usesFormOfWrapped() {
        final Phi phi = new PhConstTest.Dummy("some");
        MatcherAssert.assertThat(
            "Const object should use form of wrapped object",
            new PhConst(phi).forma(),
            Matchers.equalTo(phi.forma())
        );
    }

    @Test
    void equalsToWrapped() {
        final Phi phi = new PhConstTest.Dummy("some");
        MatcherAssert.assertThat(
            "Const object should use 'equals' of wrapped object",
            new PhConst(phi),
            Matchers.equalTo(phi)
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
                    self -> new Data.ToPhi(new SecureRandom().nextDouble())
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
            this.add(name, new AtComposite(this, rho -> this.attr(Attr.PHI).get()));
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
     * Enveloped dummy Phi.
     * @since 1.0
     */
    private static class EnvelopedDummy extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         * @param dummy Dummy to be enveloped
         */
        EnvelopedDummy(final Phi sigma, final Phi dummy) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new Data.ToPhi(
                        new Dataized(
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
                        ).take(Long.class)
                    )
                )
            );
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
            this.add("φ", new AtComposite(this, rho -> rho.attr("x").get()));
        }
    }
}
