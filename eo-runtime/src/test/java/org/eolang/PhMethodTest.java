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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhMethod}.
 *
 * @since 0.16
 */
public final class PhMethodTest {

    @Test
    public void comparesTwoObjects() {
        final Phi num = new Data.ToPhi(1L);
        MatcherAssert.assertThat(
            num.attr("plus").get(),
            Matchers.equalTo(num.attr("plus").get())
        );
    }

    @Test
    public void safeToString() {
        MatcherAssert.assertThat(
            new PhMethod(Phi.Φ, "hello").toString(),
            Matchers.endsWith(".hello")
        );
    }

    @Test
    public void calculatesPhiJustOnce() {
        final Dummy dummy = new Dummy(Phi.Φ);
        final Phi phi = new PhMethod(dummy, "φ");
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(dummy.count, Matchers.equalTo(1));
    }

    @Test
    public void calculatesToLocalJustOnce() {
        final Dummy dummy = new Dummy(Phi.Φ);
        final Phi phi = new PhMethod(dummy, "foo");
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(dummy.count, Matchers.equalTo(1));
    }

    @Test
    public void calculatesThroughPhiOnce() {
        final Dummy dummy = new Dummy(Phi.Φ);
        final Phi phi = new PhMethod(dummy, "neg");
        new Dataized(phi).take();
        MatcherAssert.assertThat(dummy.count, Matchers.equalTo(1));
    }

    @Test
    public void calculatesThroughPhiManyTimes() {
        final Dummy dummy = new Dummy(Phi.Φ);
        final Phi phi = new PhMethod(dummy, "neg");
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(dummy.count, Matchers.equalTo(total));
    }

    public static class Dummy extends PhDefault {
        public int count;
        public Dummy(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, self -> {
                ++this.count;
                return new Data.ToPhi(1L);
            }));
            this.add("foo", new AtComposite(
                this, self -> {
                ++this.count;
                return new Data.ToPhi(1L);
            }));
        }
    }

}
