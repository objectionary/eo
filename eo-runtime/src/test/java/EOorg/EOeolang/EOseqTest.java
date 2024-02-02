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

import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOseq}.
 *
 * @since 0.16
 */
public final class EOseqTest {

    @Test
    public void calculatesAndReturns() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new EOseq(Phi.Φ),
                    0,
                    new PhWith(
                        new PhWith(
                            new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                            0, new Data.ToPhi(0L)
                        ).attr("with").get().copy(),
                        0, new Data.ToPhi(1L)
                    )
                )
            ).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void calculatesAndReturnsObject() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new EOseq(Phi.Φ),
                    0,
                    new PhWith(
                        new PhWith(
                            new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                            0, new Data.ToPhi(0L)
                        ).attr("with").get().copy(),
                        0, new Data.ToPhi("Hello!")
                    )
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    /**
     * Test
     *
     * [] > parent
     *   memory 0 > counter
     *   seq > @
     *     *
     *       at.
     *         * 0 1
     *         counter.as-int
     *       counter.write (counter.as-int.plus 1)
     *       counter.as-int
     *
     * @since 1.0
     */
    @Test
    public void calculatesWithTupleAndReturnsObject() {
        final Phi counter = new PhMethod(new Parent(Phi.Φ), "counter");
        new Dataized(
            new PhWith(
                new PhCopy(
                    new PhMethod(counter, "write")
                ),
                0, new Data.ToPhi(0L)
            )
        ).take();
        final Phi get = new PhWith(
            new PhWith(
                new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                0, new Data.ToPhi(0L)
            ).attr("with").get().copy(),
            0, new Data.ToPhi(1L)
        ).attr("at").get().copy();
        get.attr(0).put(counter.attr("as-int").get());
        final Phi increment = new PhWith(
            new PhCopy(
                new PhMethod(counter, "write")
            ),
            0,
            new PhWith(
                counter.attr("as-int").get().attr("plus").get(),
                0,
                new Data.ToPhi(1L)
            )
        );
        MatcherAssert.assertThat(
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
                        counter.attr("as-int").get()
                    )
                )
            ).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    /**
     * Parent Phi.
     *
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
                "counter",
                new AtComposite(
                    this,
                    EOmemory::new
                )
            );
        }
    }

}
