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

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
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
                            new EOtuple$EOempty(Phi.Φ).take("with").copy(),
                            0, new Data.ToPhi(0L)
                        ).take("with").copy(),
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
                            new EOtuple$EOempty(Phi.Φ).take("with").copy(),
                            0, new Data.ToPhi(0L)
                        ).take("with").copy(),
                        0, new Data.ToPhi("Hello!")
                    )
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    /**
     * Test.
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
    @Disabled
    @Test
    public void calculatesWithTupleAndReturnsObject() {
        final Phi counter = new EOmemory(Phi.Φ);
        counter.attr(0).put(new Data.ToPhi(0L));
        final Phi arr = new PhWith(
            new PhWith(
                new EOtuple$EOempty(Phi.Φ).attr("with").get().copy(),
                0, new Data.ToPhi(0L)
            ).attr("with").get().copy(),
            0, new Data.ToPhi(1L)
        );
        final Phi get = new PhWith(
            new PhCopy(new PhMethod(arr, "at")),
            0, new PhMethod(counter, "as-int")
        );
        final Phi increment = new PhWith(
            new PhCopy(new PhMethod(counter, "write")),
            0,
            new PhWith(
                new PhCopy(new PhMethod(new PhMethod(counter, "as-int"), "plus")),
                0, new Data.ToPhi(1L)
            )
        );
        final Phi args = new PhWith(
            new PhCopy(
                new PhMethod(
                    new PhWith(
                        new PhCopy(
                            new PhMethod(
                                new PhWith(
                                    new PhCopy(
                                        new PhMethod(new EOtuple$EOempty(Phi.Φ), "with")
                                    ),
                                    0,
                                    get
                                ),
                                "with"
                            )
                        ),
                        0,
                        increment
                    ),
                    "with"
                )
            ),
            0, new PhMethod(counter, "as-int")
        );
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(new EOseq(Phi.Φ), 0, args)
            ).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    /**
     * Test.
     *
     * [] > parent
     *   memory 0 > counter
     *   seq > @
     *     *
     *       counter.write (counter.as-int.plus 1)
     *       counter.as-int
     *
     * @since 1.0
     */
    @Disabled
    @Test
    public void calculatesWithoutTupleAndReturnsObject() {
        final Phi counter = new EOmemory(Phi.Φ);
        counter.attr(0).put(new Data.ToPhi(0L));
        final Phi increment = new PhWith(
            new PhCopy(new PhMethod(counter, "write")),
            0,
            new PhWith(
                new PhCopy(new PhMethod(new PhMethod(counter, "as-int"), "plus")),
                0, new Data.ToPhi(1L)
            )
        );
        final Phi args = new PhWith(
            new PhCopy(
                new PhMethod(
                    new PhWith(
                        new PhCopy(
                            new PhMethod(new EOtuple$EOempty(Phi.Φ), "with")
                        ),
                        0,
                        increment
                    ),
                    "with"
                )
            ),
            0, new PhMethod(counter, "as-int")
        );
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(new EOseq(Phi.Φ), 0, args)
            ).take(Long.class),
            Matchers.equalTo(1L)
        );
    }
}
