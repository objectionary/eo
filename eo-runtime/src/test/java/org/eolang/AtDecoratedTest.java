/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link AtDecorated}.
 *
 * @since 0.16
 */
@Execution(ExecutionMode.SAME_THREAD)
public final class AtDecoratedTest {

    @Test
    public void readsOnlyOnce() {
        Dummy.count = 0;
        final Phi phi = new Dummy(Phi.Φ);
        final Phi neg = phi.attr("neg").get();
        neg.attr("Δ").get();
        neg.attr("Δ").get();
        MatcherAssert.assertThat(
            Dummy.count,
            Matchers.equalTo(3)
        );
    }

    @Test
    public void readsManyTimes() {
        Dummy.count = 0;
        final Phi phi = new Dummy(Phi.Φ);
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            phi.attr("neg").get().attr("Δ").get();
        }
        MatcherAssert.assertThat(
            Dummy.count,
            Matchers.equalTo(total * 2)
        );
    }

    public static class Dummy extends PhDefault {
        public static int count;
        public Dummy(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, self -> {
                ++Dummy.count;
                return new Data.ToPhi(1L);
            }));
        }
    }

}
