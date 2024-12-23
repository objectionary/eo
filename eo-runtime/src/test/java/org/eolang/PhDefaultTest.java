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

import com.yegor256.Together;
import java.security.SecureRandom;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault}.
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class PhDefaultTest {
    /**
     * Name of attribute.
     */
    private static final String PLUS_ATT = "plus";

    /**
     * Name of attribute.
     */
    private static final String VOID_ATT = "void";

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
        final Phi phi = new PhLocated(new PhDefaultTest.Int());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Attr.RHO),
            String.format("Object should not have %s attribute when it's just formed", Attr.RHO)
        );
    }

    @Test
    void setsRhoAfterDispatch() {
        final Phi kid = new PhDefaultTest.Int().take(PhDefaultTest.PLUS_ATT);
        Assertions.assertDoesNotThrow(
            () -> kid.take(Attr.RHO),
            String.format("Kid of should have %s attribute after dispatch", Attr.RHO)
        );
    }

    @Test
    void doesNotHaveRhoAfterCopying() {
        final Phi phi = new PhLocated(new PhDefaultTest.Int().copy());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Attr.RHO),
            String.format("Object should not give %s attribute after copying", Attr.RHO)
        );
    }

    @Test
    void copiesKid() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.take(PhDefaultTest.PLUS_ATT);
        final Phi second = phi.copy().take(PhDefaultTest.PLUS_ATT);
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
            phi.take(PhDefaultTest.PLUS_ATT),
            Matchers.not(
                Matchers.equalTo(phi.take(PhDefaultTest.PLUS_ATT))
            )
        );
    }

    @Test
    void hasKidWithSetRhoAfterCopying() {
        final Phi phi = new PhDefaultTest.Int().copy();
        final Phi plus = phi.take(PhDefaultTest.PLUS_ATT);
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
            first.take(PhDefaultTest.PLUS_ATT),
            Matchers.not(
                Matchers.equalTo(second.take(PhDefaultTest.PLUS_ATT))
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
            phi.take(PhDefaultTest.PLUS_ATT).take(Attr.RHO),
            Matchers.not(Matchers.equalTo(copy.take(PhDefaultTest.PLUS_ATT).take(Attr.RHO)))
        );
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied object kid should refer to copied object",
                Attr.RHO
            ),
            copy.take(PhDefaultTest.PLUS_ATT).take(Attr.RHO),
            Matchers.equalTo(copy)
        );
    }

    @Test
    void doesNotChangeRhoAfterDirectKidCopying() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.take(PhDefaultTest.PLUS_ATT);
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
        final Phi plus = phi.take(PhDefaultTest.PLUS_ATT);
        MatcherAssert.assertThat(
            String.format("%s attributes should not be copied while dispatch", Attr.RHO),
            plus.take(Attr.RHO),
            Matchers.equalTo(plus.take(Attr.RHO))
        );
    }

    @Test
    void copiesUnsetVoidAttribute() {
        final Phi phi = new PhLocated(new PhDefaultTest.Int());
        final Phi copy = phi.copy();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> copy.take(PhDefaultTest.VOID_ATT),
            "Unset void attribute should be copied with unset value"
        );
    }

    @Test
    void copiesSetVoidAttributeOnCopy() {
        final Phi phi = new PhDefaultTest.Int();
        phi.put(PhDefaultTest.VOID_ATT, new Data.ToPhi(10L));
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            "Copied set void attribute should be different from original one",
            phi.take(PhDefaultTest.VOID_ATT),
            Matchers.not(
                Matchers.equalTo(copy.take(PhDefaultTest.VOID_ATT))
            )
        );
    }

    @Test
    void doesNotCopySetVoidAttributeWithRho() {
        final Phi phi = new PhDefaultTest.Int();
        phi.put(PhDefaultTest.VOID_ATT, new Data.ToPhi(10L));
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            phi.take(PhDefaultTest.VOID_ATT),
            Matchers.equalTo(phi.take(PhDefaultTest.VOID_ATT))
        );
    }

    @Test
    void doesNotCopyContextAttributeWithRho() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            phi.take("context"),
            Matchers.equalTo(phi.take("context"))
        );
    }

    @Test
    void hasAccessToDependentOnContextAttribute() {
        final Phi phi = new PhLocated(new PhDefaultTest.Int().copy());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Attr.PHI),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        phi.put(PhDefaultTest.VOID_ATT, new Data.ToPhi(10L));
        Assertions.assertDoesNotThrow(
            () -> phi.take(Attr.PHI),
            AtCompositeTest.TO_ADD_MESSAGE
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
            AtCompositeTest.TO_ADD_MESSAGE,
            phi.hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void createsDifferentPhiInParallel() {
        final int threads = 100;
        MatcherAssert.assertThat(
            "all objects are unique",
            new SetOf<>(
                new Together<>(
                    threads,
                    t -> new Int()
                )
            ),
            Matchers.iterableWithSize(threads)
        );
    }

    @Test
    void failsGracefullyOnMissingAttribute() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new PhLocated(new Data.ToPhi("Hey")).take("missing-attr"),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void copiesWithSetData() {
        final String data = "Hello";
        final Phi phi = new PhDefaultTest.Int();
        phi.put(0, new Data.ToPhi(data));
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(copy).asString(),
            Matchers.equalTo(data)
        );
    }

    @Test
    void setsVoidAttributeOnlyOnce() {
        final Phi num = new Data.ToPhi(42L);
        final Phi phi = new PhDefaultTest.Foo();
        phi.put(0, num);
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> phi.put(0, num),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void printsEndlessRecursionObject() {
        final Phi phi = new PhDefaultTest.EndlessRecursion();
        PhDefaultTest.EndlessRecursion.count = 2;
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(phi).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void hesPhiRecursively() {
        final Phi phi = new PhDefaultTest.RecursivePhi();
        PhDefaultTest.RecursivePhi.count = 3;
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(phi).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void cachesPhiViaNewRecursively() {
        final Phi phi = new PhDefaultTest.RecursivePhiViaNew();
        PhDefaultTest.RecursivePhiViaNew.count = 3;
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(phi).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void doesNotReadMultipleTimes() {
        final Phi phi = new PhDefaultTest.Counter();
        final long total = 2L;
        for (long idx = 0L; idx < total; ++idx) {
            new Dataized(phi).take();
        }
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(new PhMethod(phi, "count")).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    void hasTheSameFormaWithBoundedData() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Data.ToPhi(5L).forma(),
            Matchers.equalTo(new Data.ToPhi(6).forma())
        );
    }

    @Test
    void rendersFormaProperly() {
        MatcherAssert.assertThat(
            "forma of 'number' is the full name of the 'number' object",
            new Data.ToPhi(42L).forma(),
            Matchers.equalTo("org.eolang.number")
        );
    }

    @Test
    void hasDifferentFormaWithBoundedMethod() {
        final Phi five = new Data.ToPhi(5L);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            five.forma(),
            Matchers.not(
                Matchers.equalTo(
                    new PhWith(
                        five.take(PhDefaultTest.PLUS_ATT).copy(),
                        "x",
                        new Data.ToPhi(5)
                    ).forma()
                )
            )
        );
    }

    @Test
    void hasTheSameFormaWithDifferentInstances() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new PhWith(
                new Data.ToPhi(5L).take(PhDefaultTest.PLUS_ATT).copy(),
                "x",
                new Data.ToPhi(5L)
            ).forma(),
            Matchers.equalTo(
                new PhWith(
                    new Data.ToPhi(6L).take(PhDefaultTest.PLUS_ATT).copy(),
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
                        new Rnd(), PhDefaultTest.PLUS_ATT
                    ),
                    0, new Data.ToPhi(1.2)
                ),
                PhDefaultTest.PLUS_ATT
            ),
            0, new Data.ToPhi(1.2)
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(rnd).asNumber(),
            Matchers.equalTo(new Dataized(rnd).asNumber())
        );
    }

    /**
     * Rnd.
     * @since 0.1.0
     */
    private static class Rnd extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Rnd() {
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new ToPhi(new SecureRandom().nextDouble())
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
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Int() {
            this.add(PhDefaultTest.VOID_ATT, new AtVoid(PhDefaultTest.VOID_ATT));
            this.add(PhDefaultTest.PLUS_ATT, new AtSimple(new PhDefault()));
            this.add(
                Attr.PHI,
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> rho.take(PhDefaultTest.VOID_ATT)
                    )
                )
            );
            this.add(
                "context",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> {
                            final Phi plus = new Data.ToPhi(5L).take(
                                PhDefaultTest.PLUS_ATT
                            ).copy();
                            plus.put(0, new Data.ToPhi(6L));
                            return plus;
                        }
                    )
                )
            );
        }
    }

    /**
     * Foo.
     * @since 0.1.0
     */
    public static class Foo extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Foo() {
            this.add("x", new AtVoid("x"));
            this.add("kid", new AtSimple(new PhDefaultTest.Kid()));
            this.add("φ", new AtSimple(new Data.ToPhi(5L)));
        }
    }

    /**
     * Dummy.
     * @since 0.1.0
     */
    public static class Dummy extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Dummy() {
            this.add(
                Attr.PHI,
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> {
                            ++PhDefaultTest.Dummy.count;
                            return new Data.ToPhi(1L);
                        }
                    )
                )
            );
        }
    }

    /**
     * Counter.
     * @since 0.1.0
     */
    public static class Counter extends PhDefault {
        /**
         * Count.
         */
        private long count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Counter() {
            this.add(
                Attr.PHI,
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> {
                            ++this.count;
                            return new Data.ToPhi(new byte[]{(byte) 0x01});
                        }
                    )
                )
            );
            this.add("count", new AtComposite(this, rho -> new Data.ToPhi(this.count)));
        }
    }

    /**
     * Kid.
     * @since 0.1.0
     */
    public static class Kid extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Kid() {
            this.add("z", new AtVoid("z"));
            this.add(Attr.PHI, new AtSimple(new Data.ToPhi(true)));
        }
    }

    /**
     * Endless Recursion.
     * @since 0.1.0
     */
    public static class EndlessRecursion extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        EndlessRecursion() {
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
                            result = new PhCopy(new PhDefaultTest.EndlessRecursion());
                        }
                        return result;
                    }
                )
            );
        }
    }

    /**
     * Recursive Phi.
     * @since 0.1.0
     */
    public static class RecursivePhi extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        RecursivePhi() {
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
                            result = new Data.ToPhi(new Dataized(rho).asNumber());
                        }
                        return result;
                    }
                )
            );
        }
    }

    /**
     * RecursivePhiViaNew.
     * @since 0.1.0
     */
    public static class RecursivePhiViaNew extends PhDefault {
        /**
         * Count.
         */
        private static int count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        RecursivePhiViaNew() {
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
                                    new RecursivePhiViaNew()
                                ).asNumber()
                            );
                        }
                        return result;
                    }
                )
            );
        }
    }
}
