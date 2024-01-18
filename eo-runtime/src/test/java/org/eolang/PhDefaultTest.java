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

import EOorg.EOeolang.EOio.EOstdout;
import EOorg.EOeolang.EOstring$EOlength;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault}.
 *
 * @since 0.1
 */
final class PhDefaultTest {

    @Test
    void comparesTwoObjects() {
        final Phi phi = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            phi, Matchers.equalTo(phi)
        );
    }

    @Test
    void comparesTwoCopies() {
        final Phi phi = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            phi.copy(), Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void makesObjectIdentity() {
        final Phi phi = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(phi.attr("ν").get()).take(Long.class),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void createsDifferentPhiInParallel() {
        final int threads = 100;
        final Set<PhDefault> objects = ConcurrentHashMap.newKeySet();
        new Threads<>(
            threads,
            Stream.generate(
                () -> (Scalar<PhDefault>) () -> new EOstring$EOlength(Phi.Φ)
            ).limit(threads).collect(Collectors.toList())
        ).forEach(objects::add);
        MatcherAssert.assertThat(
            objects,
            Matchers.hasSize(threads)
        );
    }

    @Test
    void failsGracefullyOnMissingAttribute() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Data.ToPhi("Hey").attr("missing-attr").get()
        );
    }

    @Test
    void makesCopy() {
        final Phi num = new Data.ToPhi(42L);
        final String data = "Hello, world!";
        final Phi phi = new PhDefaultTest.Foo(Phi.Φ, data);
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
    void setsFreeAttributeOnlyOnce() {
        final Phi num = new Data.ToPhi(42L);
        final Phi phi = new PhDefaultTest.Foo(Phi.Φ);
        phi.attr(0).put(num);
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> phi.attr(0).put(num)
        );
    }

    @Test
    void takesRhoFromAttribute() {
        final Phi phi = new PhDefaultTest.Kid(new Data.ToPhi(0L));
        MatcherAssert.assertThat(
            new Dataized(phi.attr("φ").get().attr("ρ").get()).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void changesRhoOnCopy() {
        final Phi foo = new Foo(Phi.Φ);
        final Phi kid = foo.attr("kid").get();
        kid.attr("ρ").put(Phi.Φ);
        MatcherAssert.assertThat(
            kid.attr("ρ").get(),
            Matchers.not(Matchers.equalTo(foo))
        );
    }

    @Test
    void getsRhoFromPhi() {
        final Phi first = new PhDefaultTest.First(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(first).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void printsEndlessRecursionObject() {
        final Phi phi = new PhDefaultTest.EndlessRecursion(Phi.Φ);
        PhDefaultTest.EndlessRecursion.count = 2;
        MatcherAssert.assertThat(
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void cachesPhiRecursively() {
        final Phi phi = new PhDefaultTest.RecursivePhi(Phi.Φ);
        PhDefaultTest.RecursivePhi.count = 3;
        MatcherAssert.assertThat(
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void cachesPhiViaNewRecursively() {
        final Phi phi = new PhDefaultTest.RecursivePhiViaNew(Phi.Φ);
        PhDefaultTest.RecursivePhiViaNew.count = 3;
        MatcherAssert.assertThat(
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void resetsCacheOnCopy() {
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
    void readsMultipleTimes() {
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
    void readsMultipleTimesThroughAttribute() {
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

    @Test
    void hasTheSameFormaWithBoundedData() {
        MatcherAssert.assertThat(
            new Data.ToPhi(5L).forma(),
            Matchers.equalTo(new Data.ToPhi(6L).forma())
        );
    }

    @Test
    void hasDifferentFormaWithBoundedMethod() {
        final Phi five = new Data.ToPhi(5L);
        MatcherAssert.assertThat(
            five.forma(),
            Matchers.not(
                Matchers.equalTo(
                    new PhWith(
                        five.attr("plus").get().copy(),
                        "x",
                        new Data.ToPhi(5L)
                    ).forma()
                )
            )
        );
    }

    @Test
    void hasTheSameFormaWithDifferentInstances() {
        MatcherAssert.assertThat(
            new PhWith(
                new Data.ToPhi(5L).attr("plus").get().copy(),
                "x",
                new Data.ToPhi(5L)
            ).forma(),
            Matchers.equalTo(
                new PhWith(
                    new Data.ToPhi(6L).attr("plus").get().copy(),
                    "x",
                    new Data.ToPhi(6L)
                ).forma()
            )
        );
    }

    /**
     * Foo.
     * @since 1.0
     */
    public static class Foo extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Foo(final Phi sigma) {
            this(sigma, new Object());
        }

        /**
         * Ctor.
         * @param sigma Sigma
         * @param data Data
         */
        Foo(final Phi sigma, final Object data) {
            super(sigma);
            this.add("x", new AtFree());
            this.add(
                "kid",
                new AtComposite(
                    this,
                    PhDefaultTest.Kid::new
                )
            );
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new Data.ToPhi(data)
                )
            );
        }
    }

    /**
     * Dummy.
     * @since 1.0
     */
    public static class Dummy extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         * @param sigma Sigma
         */
        Dummy(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        ++PhDefaultTest.Dummy.count;
                        return new Data.ToPhi(1L);
                    }
                )
            );
        }
    }

    /**
     * Counter.
     * @since 1.0
     */
    public static class Counter extends PhDefault {
        /**
         * Count.
         */
        private long count;

        /**
         * Ctor.
         * @param sigma Sigma
         */
        Counter(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        ++this.count;
                        return new Data.ToPhi(new byte[] {(byte) 0x01});
                    }
                )
            );
            this.add(
                "count",
                new AtComposite(
                    this,
                    self -> new Data.ToPhi(this.count)
                )
            );
        }
    }

    /**
     * Kid.
     * @since 1.0
     */
    public static class Kid extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Kid(final Phi sigma) {
            super(sigma);
            this.add("z", new AtFree());
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new EOstdout(new Data.ToPhi(1L))
                )
            );
        }
    }

    /**
     * First.
     * @since 1.0
     */
    public static class First extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma
         */
        First(final Phi sigma) {
            super(sigma);
            this.add("a", new AtFree(new Data.ToPhi(1L)));
            this.add(
                "φ",
                new AtComposite(
                    this,
                    PhDefaultTest.Second::new
                )
            );
        }
    }

    /**
     * Second.
     * @since 1.0
     */
    public static class Second extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Second(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> self.attr("ρ").get().attr("a").get()
                )
            );
        }
    }

    /**
     * Endless Recursion.
     * @since 1.0
     */
    public static class EndlessRecursion extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         * @param sigma Sigma
         */
        EndlessRecursion(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        --PhDefaultTest.EndlessRecursion.count;
                        final Phi result;
                        if (PhDefaultTest.EndlessRecursion.count <= 0) {
                            result = new Data.ToPhi(0L);
                        } else {
                            result = new PhCopy(new PhDefaultTest.EndlessRecursion(self));
                        }
                        return result;
                    }
                )
            );
        }
    }

    /**
     * Recursive Phi.
     * @since 1.0
     */
    public static class RecursivePhi extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         * @param sigma Sigma
         */
        RecursivePhi(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    rho -> {
                        --PhDefaultTest.RecursivePhi.count;
                        final Phi result;
                        if (PhDefaultTest.RecursivePhi.count <= 0) {
                            result = new Data.ToPhi(0L);
                        } else {
                            result =  new Data.ToPhi(new Dataized(rho).take(Long.class));
                        }
                        return result;
                    }
                )
            );
        }
    }

    /**
     * RecursivePhiViaNew.
     * @since 1.0
     */
    public static class RecursivePhiViaNew extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         * @param sigma Sigma
         */
        RecursivePhiViaNew(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    rho -> {
                        --PhDefaultTest.RecursivePhiViaNew.count;
                        final Phi result;
                        if (PhDefaultTest.RecursivePhi.count <= 0) {
                            result = new Data.ToPhi(0L);
                        } else {
                            result = new Data.ToPhi(
                                new Dataized(
                                    new RecursivePhiViaNew(Phi.Φ)
                                ).take(Long.class)
                            );
                        }
                        return result;
                    }
                )
            );
        }
    }

}
