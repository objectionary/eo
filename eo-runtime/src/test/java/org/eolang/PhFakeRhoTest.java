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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link PhFakeRho}.
 */
public class PhFakeRhoTest {
    @Test
    void makesNewCopy() {
        final Phi fake = new PhFakeRho(Phi.Φ, Phi.Φ, Phi.Φ);
        MatcherAssert.assertThat(
            "PhFakeRho::copy() should create new object",
            fake.copy(),
            Matchers.not(
                Matchers.equalTo(fake)
            )
        );
    }

    @Test
    void retrievesRegularAttributeFromOrigin() {
        final Phi fake = new PhFake(() -> new Data.ToPhi(10L));
        final Phi phi = new PhFakeRho(fake, Phi.Φ, Phi.Φ);
        MatcherAssert.assertThat(
            "PhFakeRho should return attribute from origin",
            phi.attr(Attr.PHI),
            Matchers.equalTo(fake.attr(Attr.PHI))
        );
    }

    @Test
    void wrapsRhoAttributeWithAtFakeRho() {
        MatcherAssert.assertThat(
            "PhFakeRho should wrap \\rho attribute with AtFakeRho",
            new PhFakeRho(
                new PhFake(() -> new Data.ToPhi(10L)),
                Phi.Φ,
                Phi.Φ
            ).attr(Attr.RHO),
            Matchers.instanceOf(AtFakeRho.class)
        );
    }

    @Test
    void fakesResultRhoAttribute() {
        final Phi ten = new Data.ToPhi(10L);
        final Phi plus = ten.attr("plus").get();
        final Phi str = new Data.ToPhi("Hello");
        MatcherAssert.assertThat(
            "Rho attribute of 10.plus should be faked to 'Hello'",
            new PhFakeRho(plus, ten, str).attr(Attr.RHO).get(),
            Matchers.equalTo(str)
        );
    }
}
