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

import EOorg.EOeolang.EOtxt.EOsprintf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhConst}.
 *
 * @since 0.16
 */
public final class PhConstTest {

    @Test
    public void makesObjectConstant() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhConst(
                    new PhWith(
                        new EOsprintf(new PhEta()),
                        0, new Data.ToPhi("Hello, world!")
                    )
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    public void caclulatesPhiOnlyOnce() {
        final Dummy dummy = new Dummy("any");
        final Phi phi = new PhConst(dummy);
        for (int idx = 0; idx < 10; ++idx) {
            MatcherAssert.assertThat(
                new Dataized(phi).take(Long.class),
                Matchers.is(1L)
            );
        }
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    public void doesntAllowAttributesOfDecorateeToBeSet() {
        final Phi phi = new Boom();
        Assertions.assertThrows(
            Attr.ReadOnlyException.class,
            () -> phi.attr("x").put(new Data.ToPhi(1L))
        );
    }

    @Test
    public void makesRhoConstToo() {
        final String name = "kid";
        final Dummy dummy = new Dummy(name);
        final Phi mtd = new PhMethod(new PhConst(dummy), name);
        for (int idx = 0; idx < 10; ++idx) {
            MatcherAssert.assertThat(
                new Dataized(mtd).take(Long.class),
                Matchers.is(1L)
            );
        }
        MatcherAssert.assertThat(
            dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    public void keepsDecorateeConst() {
        final Boom boom = new Boom();
        Phi cnst = new PhConst(boom);
        for (int idx = 0; idx < 10; ++idx) {
            final Phi phi = cnst.attr("φ").get().copy(new PhEta());
            phi.attr("x").put(new Data.ToPhi(1L));
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(boom.count, Matchers.equalTo(1));
    }

    private static class Dummy extends PhDefault {
        public int count;
        Dummy(final String name) {
            super();
            this.add("φ", new AtBound(new AtLambda(this, self -> {
                ++this.count;
                return new Data.ToPhi(1L);
            })));
            this.add(name, new AtBound(
                new AtLambda(this, PhConstTest.Kid::new)
            ));
        }
    }

    private static class Kid extends PhDefault {
        Kid(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
            this.add("φ", new AtBound(new AtLambda(this, self -> new Data.ToPhi(
                new Dataized(self.attr("ρ").get()).take(Long.class)
            ))));
        }
    }

    private static class Boom extends PhDefault {
        public int count;
        Boom() {
            this.add("φ", new AtBound(new AtLambda(this, self -> {
                ++this.count;
                return new Sub(self);
            })));
        }
    }

    private static class Sub extends PhDefault {
        Sub(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
            this.add("φ", new AtBound(new AtLambda(this, self -> new Data.ToPhi(1L))));
        }
    }
}
