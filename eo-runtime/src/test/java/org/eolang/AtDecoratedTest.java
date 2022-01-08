/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

/**
 * Test case for {@link AtDecorated}.
 *
 * @since 0.16
 */
public final class AtDecoratedTest {

    @Test
    public void fetchesNegOnlyOnce() {
        final AtDecoratedTest.Num num = new AtDecoratedTest.Num(Phi.Φ);
        num.attr("neg").get();
        MatcherAssert.assertThat(num.count, Matchers.equalTo(1));
    }

    @Test
    public void readsNegOnlyOnce() {
        final AtDecoratedTest.Num num = new AtDecoratedTest.Num(Phi.Φ);
        num.attr("neg").get().attr("Δ").get();
        MatcherAssert.assertThat(num.count, Matchers.equalTo(1));
    }

    @Test
    public void readsNegOnlyOnceWithPhMethod() {
        final AtDecoratedTest.Num num = new AtDecoratedTest.Num(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhMethod(
                        new PhCopy(new PhMethod(num, "neg")),
                        "eq"
                    ),
                    0,
                    new Data.ToPhi(1L)
                )
            ).take(Boolean.class),
            Matchers.equalTo(false)
        );
        MatcherAssert.assertThat(num.count, Matchers.equalTo(1));
    }

    @Test
    public void readsOnceAfterCopy() {
        final AtDecoratedTest.Num num = new AtDecoratedTest.Num(Phi.Φ);
        final Phi neg = num.attr("neg").get();
        final Phi copy = neg.copy();
        copy.attr("Δ").get();
        MatcherAssert.assertThat(num.count, Matchers.equalTo(1));
    }

    @Test
    public void readsOnceThroughEq() {
        final AtDecoratedTest.Num num = new AtDecoratedTest.Num(Phi.Φ);
        final Phi eql = num.attr("eq").get();
        eql.attr(0).put(new Data.ToPhi(1L));
        eql.attr("Δ").get();
        MatcherAssert.assertThat(num.count, Matchers.equalTo(1));
    }

    @Test
    public void readsNegManyTimes() {
        final AtDecoratedTest.Num num = new AtDecoratedTest.Num(Phi.Φ);
        final Phi neg = num.attr("neg").get();
        neg.attr("Δ").get();
        neg.attr("Δ").get();
        MatcherAssert.assertThat(num.count, Matchers.equalTo(2));
    }

    @Test
    public void readsTwice() {
        final AtDecoratedTest.Parent parent = new AtDecoratedTest.Parent(Phi.Φ);
        parent.attr("first").get();
        MatcherAssert.assertThat(parent.count, Matchers.equalTo(1));
    }

    @Test
    public void readsManyTimes() {
        final AtDecoratedTest.Parent parent = new AtDecoratedTest.Parent(Phi.Φ);
        final Attr first = parent.attr("first");
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            first.get();
        }
        MatcherAssert.assertThat(parent.count, Matchers.equalTo(1));
    }

    public static class Num extends PhDefault {
        public int count;
        public Num(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, rho -> {
                ++this.count;
                return new Data.ToPhi(1L);
            }));
        }
    }

    public static class Parent extends PhDefault {
        public int count;
        public Parent(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, rho -> {
                ++this.count;
                return new AtDecoratedTest.Kid(rho);
            }));
        }
    }

    public static class Kid extends PhDefault {
        public Kid(final Phi sigma) {
            super(sigma);
            this.add("first", new AtComposite(this, rho ->
                rho.attr("ρ").get().attr("second").get()));
            this.add("second", new AtComposite(this, rho ->
                new Data.ToPhi(1L)));
        }
    }
}
