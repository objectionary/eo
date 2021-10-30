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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtLambda}.
 *
 * @since 0.16
 */
public final class AtLambdaTest {

    @Test
    public void passesSelfCorrectly() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhConst(dummy);
        phi.attr("φ").get();
        MatcherAssert.assertThat(
            dummy.self,
            Matchers.equalTo(phi)
        );
    }

    @Test
    @Disabled
    public void passesSelfCorrectlyThroughChild() {
        final Dummy dummy = new Dummy();
        final Phi phi = new PhConst(dummy);
        phi.attr("Δ").get();
        MatcherAssert.assertThat(
            dummy.self,
            Matchers.equalTo(phi)
        );
    }

    private static class Dummy extends PhDefault {
        public Phi self;
        Dummy() {
            super();
            this.add("φ", new AtLambda(this, rho -> {
                this.self = rho;
                return new Data.ToPhi(1L);
            }));
        }
    }
}
