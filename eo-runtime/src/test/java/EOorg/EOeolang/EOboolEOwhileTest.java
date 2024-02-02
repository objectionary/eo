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

/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package EOorg.EOeolang;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhFake;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EObool}.
 *
 * [] > parent
 *   memory TRUE > toggle
 *   toggle.while > @
 *     [x] > kid
 *       ^.^.write FALSE > @
 *
 * @since 0.1
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EOboolEOwhileTest {

    @Test
    void iteratesOnce() {
        final AtomicBoolean term = new AtomicBoolean(true);
        final AtomicLong body = new AtomicLong(0L);
        new Dataized(
            new PhWith(
                new PhCopy(
                    new PhMethod(
                        new PhFake(
                            () -> new Data.ToPhi(
                                term.getAndSet(false)
                            )
                        ),
                        "while"
                    )
                ),
                0,
                new PhFake(() -> new Data.ToPhi(body.incrementAndGet()))
            )
        ).take();
        MatcherAssert.assertThat(
            body.get(), Matchers.equalTo(1L)
        );
    }

    @Test
    void iteratesManyTimes() {
        final long total = 5L;
        final AtomicLong term = new AtomicLong(total);
        final AtomicLong body = new AtomicLong(0L);
        new Dataized(
            new PhWith(
                new PhCopy(
                    new PhMethod(
                        new PhFake(
                            () -> new Data.ToPhi(
                                term.getAndDecrement() > 0L
                            )
                        ),
                        "while"
                    )
                ),
                0,
                new PhFake(() -> new Data.ToPhi(body.incrementAndGet()))
            )
        ).take();
        MatcherAssert.assertThat(
            body.get(), Matchers.equalTo(total)
        );
    }

    @Test
    void loopsOverAbstractObjects() {
        final Phi parent = new Parent(Phi.Φ);
        final Phi toggle = new PhCopy(new PhMethod(parent, "toggle"));
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(toggle, "write")),
                0, new Data.ToPhi(true)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    toggle.attr("as-bool").get().attr("while").get().copy(),
                    0, new Kid(Phi.Φ, toggle)
                )
            ).take(),
            Matchers.notNullValue()
        );
    }

    @Test
    void dataizesComplexBooleanToggle() {
        final Phi toggle = new PhMethod(new Parent(Phi.Φ), "toggle");
        new Dataized(
            new PhWith(
                new PhCopy(
                    new PhMethod(toggle, "write")
                ),
                0, new Data.ToPhi(true)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhMethod(
                        new PhWith(
                            new PhCopy(new PhMethod(toggle, "eq")),
                            0, new Data.ToPhi(true)
                        ),
                        "while"
                    ),
                    0, new Kid(Phi.Φ, toggle)
                )
            ).take(),
            Matchers.notNullValue()
        );
    }

    /**
     * Testing loop over a tuple
     *
     * [] > complex-parent
     *   memory 0 > counter
     *   while. > @
     *     counter.as-int.lt 1
     *     [i] > complex-kid
     *       seq > @
     *         *
     *           at.
     *             * 0 1
     *             counter.as-int
     *           counter.write (counter.as-int.plus 1)
     *           counter.as-int
     */
    @Test
    void loopsOverTupleComplex() {
        final Phi counter = new PhMethod(new ComplexParent(Phi.Φ), "counter");
        new Dataized(
            new PhWith(
                new PhCopy(
                    new PhMethod(counter, "write")
                ),
                0, new Data.ToPhi(0L)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhMethod(
                        new PhWith(
                            new PhCopy(new PhMethod(counter.attr("as-int").get(), "lt")),
                            0, new Data.ToPhi(1L)
                        ),
                        "while"
                    ),
                    0, new ComplexKid(Phi.Φ, counter)
                )
            ).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    /**
     * Parent Phi.
     * @since 1.0
     */
    private static final class Parent extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Parent(final Phi sigma) {
            super(sigma);
            this.add(
                "toggle",
                new AtComposite(
                    this,
                    EOmemory::new
                )
            );
        }
    }

    /**
     * Kid Phi.
     * @since 1.0
     */
    private static final class Kid extends PhDefault {
        /**
         * Toggle.
         */
        private final Phi toggle;

        /**
         * Ctor.
         * @param sigma Sigma
         * @param tgl Toggle
         */
        Kid(final Phi sigma, final Phi tgl) {
            super(sigma);
            this.toggle = tgl;
            this.add("x", new AtFree());
            this.add(
                "φ",
                new AtComposite(
                    this,
                    rho -> {
                        new Dataized(
                            new PhWith(
                                new PhCopy(
                                    new PhMethod(this.toggle, "write")
                                ),
                                0,
                                new Data.ToPhi(false)
                            )
                        ).take();
                        return new Data.ToPhi(1L);
                    }
                )
            );
        }
    }

    /**
     * Complex Parent Phi.
     *
     * @since 1.0
     */
    private static final class ComplexParent extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        ComplexParent(final Phi sigma) {
            super(sigma);
            this.add(
                "counter",
                new AtComposite(
                    this,
                    EOmemory::new
                )
            );
        }
    }

    /**
     * Complex Kid Phi.
     *
     * @since 0.1
     * @checkstyle TypeNameCheck (4 lines)
     */
    private static final class ComplexKid extends PhDefault {
        /**
         * Counter.
         */
        private final Phi counter;

        /**
         * Ctor.
         * @param sigma Sigma
         * @param ctr Counter
         */
        ComplexKid(final Phi sigma, final Phi ctr) {
            super(sigma);
            this.counter = ctr;
            this.add("i", new AtFree());
            final Phi get = new PhWith(
                new PhWith(
                    new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                    0, new Data.ToPhi(0L)
                ).attr("with").get().copy(),
                0, new Data.ToPhi(1L)
            ).attr("at").get().copy();
            get.attr(0).put(this.counter.attr("as-int").get());
            final Phi increment = new PhWith(
                new PhCopy(
                    new PhMethod(this.counter, "write")
                ),
                0,
                new PhWith(
                    this.counter.attr("as-int").get().attr("plus").get(),
                    0,
                    new Data.ToPhi(1L)
                )
            );
            this.add(
                "φ",
                new AtComposite(
                    this,
                    rho -> new Data.ToPhi(
                        new Dataized(
                            new PhWith(
                                new EOseq(Phi.Φ),
                                0,
                                new PhWith(
                                    new PhWith(
                                        new PhWith(
                                            new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                                            0,
                                            get
                                        ).attr("with").get().copy(),
                                        0,
                                        increment
                                    ).attr("with").get().copy(),
                                    0,
                                    this.counter.attr("as-int").get()
                                )
                            )
                        ).take(Long.class)
                    )
                )
            );
        }
    }
}
