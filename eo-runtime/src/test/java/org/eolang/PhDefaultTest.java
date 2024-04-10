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

import EOorg.EOeolang.EOerror;
import EOorg.EOeolang.EOio.EOstdout;
import java.security.SecureRandom;
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
 * @since 0.1
 */
final class PhDefaultTest {

    @Test
    void comparesTwoObjects() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            "Object should be equal to itself",
            phi, Matchers.equalTo(phi)
        );
    }

    @Test
    void comparesSelfToCopy() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            "Object should not be equal to its copy",
            phi, Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void comparesTwoCopies() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            "Two copies of object should be equal to each other",
            phi.copy(), Matchers.not(Matchers.equalTo(phi.copy()))
        );
    }

    @Test
    void doesNotHaveRhoWhenFormed() {
        final Phi phi = new PhDefaultTest.Int();
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> phi.take(Attr.RHO),
            String.format("Object should not have %s attribute when it's just formed", Attr.RHO)
        );
    }

    @Test
    void setsRhoAfterDispatch() {
        final Phi kid = new PhDefaultTest.Int().take("plus");
        Assertions.assertDoesNotThrow(
            () -> kid.take(Attr.RHO),
            String.format("Kid of should have %s attribute after dispatch", Attr.RHO)
        );
    }

    @Test
    void doesNotHaveRhoAfterCopying() {
        final Phi phi = new PhDefaultTest.Int().copy();
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> phi.take(Attr.RHO),
            String.format("Object should not give %s attribute after copying", Attr.RHO)
        );
    }

    @Test
    void copiesKid() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.take("plus");
        final Phi second = phi.copy().take("plus");
        MatcherAssert.assertThat(
            "Child attributes should be copied after copying main object",
            first,
            Matchers.not(
                Matchers.equalTo(second)
            )
        );
    }

    @Test
    void takesDifferentAbstractKidsEveryDispatch() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            "Child attributes should be copied on every dispatch",
            phi.take("plus"),
            Matchers.not(
                Matchers.equalTo(phi.take("plus"))
            )
        );
    }

    @Test
    void hasKidWithSetRhoAfterCopying() {
        final Phi phi = new PhDefaultTest.Int().copy();
        final Phi plus = phi.take("plus");
        Assertions.assertDoesNotThrow(
            () -> plus.take(Attr.RHO),
            String.format(
                "Child object should get %s attribute after copying main object",
                Attr.RHO
            )
        );
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied child object should be equal to copied main object",
                Attr.RHO
            ),
            plus.take(Attr.RHO),
            Matchers.equalTo(phi)
        );
    }

    @Test
    void hasDifferentKidsAfterDoubleCopying() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.copy();
        final Phi second = first.copy();
        MatcherAssert.assertThat(
            "Child objects after double copying should be different",
            first.take("plus"),
            Matchers.not(
                Matchers.equalTo(second.take("plus"))
            )
        );
    }

    @Test
    void changesKidRhoAfterSelfCopying() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of original object kid should refer to original object", Attr.RHO
            ),
            phi.take("plus").take(Attr.RHO),
            Matchers.not(Matchers.equalTo(copy.take("plus").take(Attr.RHO)))
        );
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied object kid should refer to copied object",
                Attr.RHO
            ),
            copy.take("plus").take(Attr.RHO),
            Matchers.equalTo(copy)
        );
    }

    @Test
    void doesNotChangeRhoAfterDirectKidCopying() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.take("plus");
        final Phi second = first.copy();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of kid attribute should not be changed after direct copying",
                Attr.RHO
            ),
            first.take(Attr.RHO),
            Matchers.equalTo(
                second.take(Attr.RHO)
            )
        );
    }

    @Test
    void doesNotCopyRhoWhileDispatch() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi plus = phi.take("plus");
        MatcherAssert.assertThat(
            String.format("%s attributes should not be copied while dispatch", Attr.RHO),
            plus.take(Attr.RHO),
            Matchers.equalTo(plus.take(Attr.RHO))
        );
    }

    @Test
    void doesNotCopySigmaWhileDispatch() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi plus = phi.take("plus");
        MatcherAssert.assertThat(
            String.format("%s attributes should not be copied while dispatch", Attr.SIGMA),
            plus.take(Attr.SIGMA),
            Matchers.equalTo(plus.take(Attr.SIGMA))
        );
    }

    @Test
    void copiesUnsetVoidAttribute() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi copy = phi.copy();
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> copy.take("void"),
            "Unset void attribute should be copied with unset value"
        );
    }

    @Test
    void copiesSetVoidAttributeOnCopy() {
        final Phi phi = new PhDefaultTest.Int();
        phi.put("void", new Data.ToPhi(10L));
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            "Copied set void attribute should be different from original one",
            phi.take("void"),
            Matchers.not(
                Matchers.equalTo(copy.take("void"))
            )
        );
    }

    @Test
    void doesNotCopySetVoidAttributeWithRho() {
        final Phi phi = new PhDefaultTest.Int();
        phi.put("void", new Data.ToPhi(10L));
        MatcherAssert.assertThat(
            phi.take("void"),
            Matchers.equalTo(phi.take("void"))
        );
    }

    @Test
    void doesNotCopyContextAttributeWithRho() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            phi.take("context"),
            Matchers.equalTo(phi.take("context"))
        );
    }

    @Test
    void hasAccessToDependentOnContextAttribute() {
        final Phi phi = new PhDefaultTest.Int().copy();
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> phi.take(Attr.PHI)
        );
        phi.put("void", new Data.ToPhi(10L));
        Assertions.assertDoesNotThrow(
            () -> phi.take(Attr.PHI)
        );
    }

    @Test
    void hasContextedChildWithSetRhoWhenFormed() {
        final Phi phi = new PhDefaultTest.Int();
        Assertions.assertDoesNotThrow(
            () -> phi.take("context").take(Attr.RHO),
            String.format(
                "Contexted attribute should already have %s attribute",
                Attr.RHO
            )
        );
    }

    @Test
    void makesObjectIdentity() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            phi.hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void createsDifferentPhiInParallel() {
        final int threads = 100;
        final Set<PhDefault> objects = ConcurrentHashMap.newKeySet();
        new Threads<>(
            threads,
            Stream.generate(
                () -> (Scalar<PhDefault>) Int::new
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
            EOerror.ExError.class,
            () -> new Data.ToPhi("Hey").take("missing-attr")
        );
    }

    @Test
    void copiesWithSetData() {
        final String data = "Hello";
        final Phi phi = new PhDefaultTest.Int();
        phi.put(0, new Data.ToPhi(data));
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            new Dataized(copy).take(String.class),
            Matchers.equalTo(data)
        );
    }

    @Test
    void setsVoidAttributeOnlyOnce() {
        final Phi num = new Data.ToPhi(42L);
        final Phi phi = new PhDefaultTest.Foo(Phi.Φ);
        phi.put(0, num);
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> phi.put(0, num)
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
    void refersToOriginalObjectAndDoesNotResetCache() {
        final Phi phi = new PhDefaultTest.Dummy(Phi.Φ);
        phi.take("plus");
        final Phi copy = phi.copy();
        copy.take("plus");
        phi.take("plus");
        MatcherAssert.assertThat(
            PhDefaultTest.Dummy.count,
            Matchers.equalTo(1)
        );
    }

    @Test
    void doesNotReadMultipleTimes() {
        final Phi phi = new PhDefaultTest.Counter(Phi.Φ);
        final long total = 2L;
        for (long idx = 0L; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(
            new Dataized(new PhMethod(phi, "count")).take(Long.class),
            Matchers.equalTo(1L)
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
                        five.take("plus").copy(),
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
                new Data.ToPhi(5L).take("plus").copy(),
                "x",
                new Data.ToPhi(5L)
            ).forma(),
            Matchers.equalTo(
                new PhWith(
                    new Data.ToPhi(6L).take("plus").copy(),
                    "x",
                    new Data.ToPhi(6L)
                ).forma()
            )
        );
    }

    @Test
    void doesNotCalculateRandomTwice() {
        final Phi rnd = new PhWith(
            new PhMethod(
                new PhWith(
                    new PhMethod(
                        new Rnd(Phi.Φ), "plus"
                    ),
                    0, new Data.ToPhi(1.2)
                ),
                "plus"
            ),
            0, new Data.ToPhi(1.2)
        );
        MatcherAssert.assertThat(
            new Dataized(rnd).take(Double.class),
            Matchers.equalTo(new Dataized(rnd).take(Double.class))
        );
    }

    @Test
    void injectsDeltaIntoTerm() {
        MatcherAssert.assertThat(
            new Data.ToPhi(new byte[] {0x01, 0x02, 0x03}).φTerm(),
            Matchers.containsString("Δ ↦ 01-02-03")
        );
    }

    /**
     * Rnd.
     * @since 1.0
     */
    private static class Rnd extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Rnd(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        return new Data.ToPhi(new SecureRandom().nextDouble());
                    }
                )
            );
        }
    }

    /**
     * Int.
     * @since 0.36.0
     */
    private static class Int extends PhDefault {
        /**
         * Ctor.
         */
        Int() {
            super(Phi.Φ);
            this.add("void", new AtVoid("void"));
            this.add("plus", new AtSimple(new Plus(this)));
            this.add(
                Attr.PHI,
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> rho.take("void")
                    )
                )
            );
            this.add(
                "context",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> {
                            final Phi plus = new Data.ToPhi(5L).take("plus").copy();
                            plus.put(0, new Data.ToPhi(6L));
                            return plus;
                        }
                    )
                )
            );
        }
    }

    /**
     * Plus.
     * @since 0.36.0
     */
    private static class Plus extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Plus(final Phi sigma) {
            super(sigma);
        }
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
            this(sigma, 5L);
        }

        /**
         * Ctor.
         * @param sigma Sigma
         * @param data Data
         */
        Foo(final Phi sigma, final Object data) {
            super(sigma);
            this.add("x", new AtVoid("x"));
            this.add("kid", new AtSimple(new PhDefaultTest.Kid(this)));
            this.add("φ", new AtSimple(new Data.ToPhi(data)));
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
                Attr.PHI,
                new AtFormed(
                    () -> {
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
                Attr.PHI,
                new AtFormed(
                    () -> {
                        ++this.count;
                        return new Data.ToPhi(new byte[]{(byte) 0x01});
                    }
                )
            );
            this.add("count", new AtFormed(() -> new Data.ToPhi(this.count)));
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
            this.add("z", new AtVoid("z"));
            this.add(Attr.PHI, new AtSimple(new EOstdout(Phi.Φ)));
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
                Attr.PHI,
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
                            result = new Data.ToPhi(new Dataized(rho).take(Long.class));
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
                new AtFormed(
                    () -> {
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
