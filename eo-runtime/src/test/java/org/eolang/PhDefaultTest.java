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

import EOorg.EOeolang.EOstring;
import EOorg.EOeolang.EOtxt.EOsprintf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault}.
 *
 * @since 0.1
 */
public final class PhDefaultTest {

    @Test
    public void comparesTwoObjects() {
        final Phi phi = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            phi, Matchers.equalTo(phi)
        );
    }

    @Test
    public void comparesTwoCopies() {
        final Phi phi = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            phi.copy(), Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    public void makesObjectIdentity() {
        final Phi phi = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(phi.attr("ν").get()).take(Long.class),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    public void failsGracefullyOnMissingAttribute() {
        final ExUnset error = Assertions.assertThrows(
            ExUnset.class,
            () -> new EOstring(Phi.Φ).attr("missing-attr").get()
        );
        MatcherAssert.assertThat(
            error.getMessage(),
            Matchers.containsString(
                "Error at \"EOorg.EOeolang.EOstring#missing-attr\" attribute"
            )
        );
    }

    @Test
    public void makesCopy() {
        final Phi num = new Data.ToPhi(42L);
        final Phi parent = new EOsprintf(Phi.Φ);
        final String data = "Hello, world!";
        final Phi phi = new PhDefaultTest.Foo(parent, data);
        phi.attr(0).put(num);
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            new Dataized(copy).take(String.class),
            Matchers.equalTo(data)
        );
        MatcherAssert.assertThat(
            phi.attr("x").get().attr("Δ"),
            Matchers.notNullValue()
        );
    }

    @Test
    public void setsFreeAttributeOnlyOnce() {
        final Phi num = new Data.ToPhi(42L);
        final Phi phi = new PhDefaultTest.Foo(Phi.Φ);
        phi.attr(0).put(num);
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> phi.attr(0).put(num)
        );
    }

    @Test
    public void takesRhoFromAttribute() {
        final Phi phi = new PhDefaultTest.Kid(new Data.ToPhi(0L));
        MatcherAssert.assertThat(
            new Dataized(phi.attr("φ").get().attr("ρ").get()).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void changesRhoOnCopy() {
        final Phi foo = new Foo(Phi.Φ);
        final Phi kid = foo.attr("kid").get();
        kid.move(Phi.Φ);
        MatcherAssert.assertThat(
            kid.attr("ρ").get(),
            Matchers.not(Matchers.equalTo(foo))
        );
    }

    @Test
    public void getsRhoFromPhi() {
        final Phi first = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(first).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void printsEndlessRecursionObject() {
        final Phi phi = new PhDefaultTest.EndlessRecursion(Phi.Φ);
        PhDefaultTest.EndlessRecursion.count = 2;
        MatcherAssert.assertThat(
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    public void recursiveCachingOfPhi() {
        final Phi phi = new PhDefaultTest.RecursivePhi(Phi.Φ);
        PhDefaultTest.RecursivePhi.count = 3;
        MatcherAssert.assertThat(
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    public void recursiveCachingOfPhiViaNew() {
        final Phi phi = new PhDefaultTest.RecursivePhiViaNew(Phi.Φ);
        PhDefaultTest.RecursivePhiViaNew.count = 3;
        MatcherAssert.assertThat(
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    public void resetsCacheOnCopy() {
        final Phi phi = new PhDefaultTest.Dummy(Phi.Φ);
        phi.attr("plus").get();
        final Phi copy = phi.copy();
        copy.attr("plus").get();
        phi.attr("plus").get();
        MatcherAssert.assertThat(
            PhDefaultTest.Dummy.count,
            Matchers.equalTo(2)
        );
    }

    @Test
    public void readsMultipleTimes() {
        final Phi phi = new PhDefaultTest.Counter(Phi.Φ);
        final long total = 2L;
        for (long idx = 0L; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(
            new Dataized(new PhMethod(phi, "count")).take(Long.class),
            Matchers.equalTo(total)
        );
    }

    @Test
    public void readsMultipleTimesThroughAttribute() {
        final Phi phi = new PhDefaultTest.Counter(Phi.Φ);
        final Phi eql = phi.attr("eq").get().copy();
        eql.attr(0).put(new Data.ToPhi(true));
        final long total = 3L;
        for (long idx = 0L; idx < total; ++idx) {
            eql.attr("Δ").get();
        }
        MatcherAssert.assertThat(
            new Dataized(new PhMethod(phi, "count")).take(Long.class),
            Matchers.equalTo(total)
        );
    }

    public static class Foo extends PhDefault {
         public Foo(final Phi sigma) {
             this(sigma, new Object());
         }
         public Foo(final Phi sigma, final Object data) {
             super(sigma);
             this.add("x", new AtFree());
             this.add("kid", new AtComposite(
                 this, PhDefaultTest.Kid::new
             ));
             this.add("φ", new AtComposite(
                 this, self -> new Data.ToPhi(data)
             ));
        }
    }

    public static class Dummy extends PhDefault {
        public static int count;
        public Dummy(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, self -> {
                    ++PhDefaultTest.Dummy.count;
                    return new Data.ToPhi(1L);
                }
            ));
        }
    }

    public static class Counter extends PhDefault {
        private long count;
        public Counter(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, self -> {
                    ++this.count;
                    return new Data.ToPhi(new byte[] { (byte) 0x01 });
                }
            ));
            this.add("count", new AtComposite(
                this, self -> new Data.ToPhi(this.count)
            ));
        }
    }

    public static class Kid extends PhDefault {
        public Kid(final Phi sigma) {
            super(sigma);
            this.add("z", new AtFree());
            this.add("φ", new AtComposite(
                this, self -> new EOsprintf(new Data.ToPhi(1L))
            ));
        }
    }

    public static class First extends PhDefault {
        public First(final Phi sigma) {
            super(sigma);
            this.add("a", new AtFree(new Data.ToPhi(1L)));
            this.add("φ", new AtComposite(
                this, PhDefaultTest.Second::new
            ));
        }
    }

    public static class Second extends PhDefault {
        public Second(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(
                this, self -> self.attr("ρ").get().attr("a").get()
            ));
        }
    }

    public static class EndlessRecursion extends PhDefault {
        public static int count;
        public EndlessRecursion(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(this, self -> {
                --PhDefaultTest.EndlessRecursion.count;
                if (PhDefaultTest.EndlessRecursion.count <= 0) {
                    return new Data.ToPhi(0L);
                }
                return new PhCopy(new PhDefaultTest.EndlessRecursion(self));
            }));
        }
    }

    public static class RecursivePhi extends PhDefault {
        public static int count;
        public RecursivePhi(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(this, rho -> {
                --PhDefaultTest.RecursivePhi.count;
                if (PhDefaultTest.RecursivePhi.count <= 0) {
                    return new Data.ToPhi(0L);
                }
                return new Data.ToPhi(new Dataized(rho).take(Long.class));
            }));
        }
    }

    public static class RecursivePhiViaNew extends PhDefault {
        public static int count;
        public RecursivePhiViaNew(final Phi sigma) {
            super(sigma);
            this.add("φ", new AtComposite(this, rho -> {
                --PhDefaultTest.RecursivePhiViaNew.count;
                if (PhDefaultTest.RecursivePhi.count <= 0) {
                    return new Data.ToPhi(0L);
                }
                return new Data.ToPhi(
                    new Dataized(
                        new RecursivePhiViaNew(Phi.Φ)
                    ).take(Long.class)
                );
            }));
        }
    }

}
