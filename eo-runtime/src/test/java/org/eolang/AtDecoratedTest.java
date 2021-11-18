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

import java.util.logging.Level;
import java.util.logging.Logger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.bridge.SLF4JBridgeHandler;

/**
 * Test case for {@link AtDecorated}.
 *
 * @since 0.16
 */
public final class AtDecoratedTest {

    @Test
    @Disabled
    public void readsOnlyOnce() {
        final Dummy dummy = new Dummy(Phi.Φ);
        final Phi neg = dummy.attr("neg").get();
        neg.attr("Δ").get();
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    @Disabled
    public void readsManyTimes() {
        final Dummy dummy = new Dummy(Phi.Φ);
        final int total = 10;
        for (int idx = 0; idx < total; ++idx) {
            dummy.attr("neg").get().attr("Δ").get();
        }
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(total)
        );
    }

    public static class Dummy extends PhDefault {
        public int count;
        public Dummy(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, rho -> {
                ++this.count;
                return new Data.ToPhi(1L);
            }));
        }
    }

}
