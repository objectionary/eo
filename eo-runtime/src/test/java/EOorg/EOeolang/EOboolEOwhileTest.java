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
 *       ^.^.write FALSE
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
}
