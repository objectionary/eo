/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import EOorg.EOeolang.EOnumber;
import com.yegor256.Together;
import java.security.SecureRandom;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
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
        final Phi phi = new PhSafe(new PhDefaultTest.Int());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Phi.RHO),
            String.format("Object should not have %s attribute when it's just formed", Phi.RHO)
        );
    }

    @Test
    void setsRhoAfterDispatch() {
        final Phi kid = new PhDefaultTest.Int().take(this.plus());
        Assertions.assertDoesNotThrow(
            () -> kid.take(Phi.RHO),
            String.format("Kid of should have %s attribute after dispatch", Phi.RHO)
        );
    }

    @Test
    void doesNotHaveRhoAfterCopying() {
        final Phi phi = new PhSafe(new PhDefaultTest.Int().copy());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Phi.RHO),
            String.format("Object should not give %s attribute after copying", Phi.RHO)
        );
    }

    @Test
    void copiesKid() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.take(this.plus());
        final Phi second = phi.copy().take(this.plus());
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
            phi.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(phi.take(this.plus()))
            )
        );
    }

    @Test
    void hasKidWithSetRhoAfterCopying() {
        final Phi phi = new PhDefaultTest.Int().copy();
        final Phi plus = phi.take(this.plus());
        Assertions.assertDoesNotThrow(
            () -> plus.take(Phi.RHO),
            String.format(
                "Child object should get %s attribute after copying main object",
                Phi.RHO
            )
        );
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied child object should be equal to copied main object",
                Phi.RHO
            ),
            plus.take(Phi.RHO),
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
            first.take(this.plus()),
            Matchers.not(
                Matchers.equalTo(second.take(this.plus()))
            )
        );
    }

    @Test
    void changesKidRhoAfterSelfCopying() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of original object kid should refer to original object", Phi.RHO
            ),
            phi.take(this.plus()).take(Phi.RHO),
            Matchers.not(Matchers.equalTo(copy.take(this.plus()).take(Phi.RHO)))
        );
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of copied object kid should refer to copied object",
                Phi.RHO
            ),
            copy.take(this.plus()).take(Phi.RHO),
            Matchers.equalTo(copy)
        );
    }

    @Test
    void doesNotChangeRhoAfterDirectKidCopying() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi first = phi.take(this.plus());
        final Phi second = first.copy();
        MatcherAssert.assertThat(
            String.format(
                "%s attribute of kid attribute should not be changed after direct copying",
                Phi.RHO
            ),
            first.take(Phi.RHO),
            Matchers.equalTo(
                second.take(Phi.RHO)
            )
        );
    }

    @Test
    void doesNotCopyRhoWhileDispatch() {
        final Phi phi = new PhDefaultTest.Int();
        final Phi plus = phi.take(this.plus());
        MatcherAssert.assertThat(
            String.format("%s attributes should not be copied while dispatch", Phi.RHO),
            plus.take(Phi.RHO),
            Matchers.equalTo(plus.take(Phi.RHO))
        );
    }

    @Test
    void copiesUnsetVoidAttribute() {
        final Phi phi = new PhSafe(new PhDefaultTest.Int());
        final Phi copy = phi.copy();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> copy.take(this.getVoid()),
            "Unset void attribute should be copied with unset value"
        );
    }

    @Test
    void copiesSetVoidAttributeOnCopy() {
        final Phi phi = new PhDefaultTest.Int();
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            "Copied set void attribute should be different from original one",
            phi.take(this.getVoid()),
            Matchers.not(
                Matchers.equalTo(copy.take(this.getVoid()))
            )
        );
    }

    @Test
    void doesNotCopySetVoidAttributeWithRho() {
        final Phi phi = new PhDefaultTest.Int();
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        MatcherAssert.assertThat(
            "Void attribute should not be copied with rho, but it did",
            phi.take(this.getVoid()),
            Matchers.equalTo(phi.take(this.getVoid()))
        );
    }

    @Disabled
    @Test
    void doesNotCopyContextAttributeWithRho() {
        final Phi phi = this.phiWithContextAttribute("context-doesNotCopyContextAttributeWithRho");
        MatcherAssert.assertThat(
            "Context attribute should not be copied with rho, but it did",
            phi.take("context-doesNotCopyContextAttributeWithRho"),
            Matchers.equalTo(phi.take("context-doesNotCopyContextAttributeWithRho"))
        );
    }

    @Test
    void hasAccessToDependentOnContextAttribute() {
        final Phi phi = new PhSafe(new PhDefaultTest.Int().copy());
        Assertions.assertThrows(
            ExAbstract.class,
            () -> phi.take(Phi.PHI),
            "Phi should not be accessible without setting void attribute, but it did"
        );
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        Assertions.assertDoesNotThrow(
            () -> phi.take(Phi.PHI),
            "Phi should be accessible after setting void attribute, but it didn't"
        );
    }

    @Test
    void hasContextedChildWithSetRhoWhenFormed() {
        final Phi phi = new PhDefaultTest.Int();
        Assertions.assertDoesNotThrow(
            () -> phi.take("context-hasContextedChildWithSetRhoWhenFormed").take(Phi.RHO),
            String.format(
                "Contexted attribute should already have %s attribute",
                Phi.RHO
            )
        );
    }

    @Test
    void makesObjectIdentity() {
        final Phi phi = new PhDefaultTest.Int();
        MatcherAssert.assertThat(
            "Object should have a hashCode greater then 0, but it didn't",
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
            () -> new PhSafe(new Data.ToPhi("Hey")).take("missing-attr"),
            "Accessing a missing attribute should fail, but it didn't"
        );
    }

    @Test
    void copiesWithSetData() {
        final String data = "Hello";
        final Phi phi = new PhDefaultTest.Int();
        phi.put(0, new Data.ToPhi(data));
        final Phi copy = phi.copy();
        MatcherAssert.assertThat(
            "Copied Phi should contain the same data, but it didn't",
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
            "Setting void attribute more than once should fail, but it didn't"
        );
    }

    @Test
    void printsEndlessRecursionObject() {
        final Phi phi = new PhDefaultTest.EndlessRecursion();
        PhDefaultTest.EndlessRecursion.count = 2;
        MatcherAssert.assertThat(
            "Dataization should discover the infinite recursion, but it didn't",
            new Dataized(phi).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void hesPhiRecursively() {
        final Phi phi = new PhDefaultTest.RecursivePhi();
        PhDefaultTest.RecursivePhi.count = 3;
        MatcherAssert.assertThat(
            "Dataization should discover the infinite recursion, but it didn't",
            new Dataized(phi).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void cachesPhiViaNewRecursively() {
        final Phi phi = new PhDefaultTest.RecursivePhiViaNew();
        PhDefaultTest.RecursivePhiViaNew.count = 3;
        MatcherAssert.assertThat(
            "Does not cache phi via new recursively",
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
            "Phi should not be read multiple times, but it was",
            new Dataized(new PhMethod(phi, "count")).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    void hasTheSameFormaWithBoundedData() {
        MatcherAssert.assertThat(
            "Bounded data objects should have the same forma, but they didn't",
            new Data.ToPhi(5L).forma(),
            Matchers.equalTo(new Data.ToPhi(6).forma())
        );
    }

    @Test
    void rendersFormaOnAnonymousAbstract() {
        MatcherAssert.assertThat(
            "Anonymous abstract object should be rendered without scopes",
            new PhDefault().forma(),
            Matchers.equalTo("[]")
        );
    }

    @Test
    void rendersFormaProperly() {
        MatcherAssert.assertThat(
            "forma of 'number' is the full name of the 'number' object",
            new Data.ToPhi(42L).forma(),
            Matchers.equalTo("Φ.org.eolang.number")
        );
    }

    @Test
    void hasDifferentFormaWithBoundedMethod() {
        final Phi five = new Data.ToPhi(5L);
        MatcherAssert.assertThat(
            "Phi and bounded method result should have different formas, but they were the same",
            five.forma(),
            Matchers.not(
                Matchers.equalTo(
                    new PhWith(
                        five.take(this.plus()).copy(),
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
            "Similar Phis with different data should have the same forma, but they didn't",
            new PhWith(
                new Data.ToPhi(5L).take(this.plus()).copy(),
                "x",
                new Data.ToPhi(5L)
            ).forma(),
            Matchers.equalTo(
                new PhWith(
                    new Data.ToPhi(6L).take(this.plus()).copy(),
                    "x",
                    new Data.ToPhi(6L)
                ).forma()
            )
        );
    }

    @Test
    void injectsPhi() {
        final Phi phi = new WithVoidPhi();
        phi.put(0, new Data.ToPhi(5));
        MatcherAssert.assertThat(
            "Object must be injected to phi attribute and dataized",
            new Dataized(phi).asNumber().intValue(),
            Matchers.equalTo(5)
        );
    }

    @Test
    void doesNotCalculateRandomTwice() {
        final Phi rnd = new PhWith(
            new PhMethod(
                new PhWith(
                    new PhMethod(
                        new Rnd(), this.plus()
                    ),
                    0, new Data.ToPhi(1.2)
                ),
                this.plus()
            ),
            0, new Data.ToPhi(1.2)
        );
        MatcherAssert.assertThat(
            "Random value should be the same on second access, but it wasn't",
            new Dataized(rnd).asNumber(),
            Matchers.equalTo(new Dataized(rnd).asNumber())
        );
    }

    @Test
    void failsCorrectlyWhenTooManyAttributesPut() {
        MatcherAssert.assertThat(
            "the message explains what's going on",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new EOnumber().put(1, new Data.ToPhi(1)),
                "fails when trying to set attribute with too big position"
            ).getMessage(),
            Matchers.containsString("Can't overwrite the cached attribute ")
        );
    }

    @Test
    void verifiesThreadLocalNesting() {
        final Phi phi = this.phiWithContextAttribute("context-verifiesThreadLocalNesting");
        Assertions.assertDoesNotThrow(
            () -> phi.take("context-verifiesThreadLocalNesting"),
            "Nesting should be properly managed without exceptions"
        );
    }

    @Test
    void verifiesThreadLocalNestingWithExceptions() {
        final Phi phi = this.phiWithContextAttribute(
            "context-verifiesThreadLocalNestingWithExceptions"
        );
        Assertions.assertThrows(
            ExUnset.class,
            () -> phi.take("non-existent-attribute"),
            "Should throw exception for non-existent attribute"
        );
        Assertions.assertDoesNotThrow(
            () -> phi.take("context-verifiesThreadLocalNestingWithExceptions"),
            "Should still work after exception"
        );
    }

    @Test
    void worksAfterCleanup() {
        final Phi phi = this.phiWithContextAttribute("context-worksAfterCleanup");
        phi.take("context-worksAfterCleanup");
        cleansUpNesting();
        Assertions.assertDoesNotThrow(
            () -> phi.take("context-worksAfterCleanup"),
            "Should work after cleanup"
        );
    }

    @Test
    void verifiesThreadLocalInMultipleThreads() {
        final int threads = 10;
        final int cnt = 100;
        final boolean[] res = new boolean[threads];
        final Thread[] pool = new Thread[threads];
        for (int idx = 0; idx < threads; idx += 1) {
            final int id = idx;
            pool[idx] = new Thread(
                () -> {
                    final Phi phi = this.phiWithContextAttribute(
                        "context-verifiesThreadLocalInMultipleThreads"
                    );
                    try {
                        for (int iter = 0; iter < cnt; iter += 1) {
                            phi.take("context-verifiesThreadLocalInMultipleThreads");
                        }
                        res[id] = true;
                    } catch (final IllegalStateException ex) {
                        res[id] = false;
                    } finally {
                        cleansUpNesting();
                    }
                }
            );
            pool[idx].start();
        }
        this.joinsThreads(pool);
        for (int idx = 0; idx < threads; idx += 1) {
            Assertions.assertTrue(
                res[idx],
                String.format("Thread %d should have completed successfully", idx)
            );
        }
    }

    private void joinsThreads(final Thread... threads) {
        for (final Thread thread : threads) {
            try {
                thread.join();
            } catch (final InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("Thread interrupted", ex);
            }
        }
    }

    private Phi phiWithContextAttribute(final String attribute) {
        final PhDefault phi = new PhDefault();
        phi.add(attribute, new AtComposite(phi, rho -> new Data.ToPhi(42)));
        return phi;
    }

    /**
     * Returns the 'plus' literal.
     */
    private String plus() {
        return "plus";
    }

    /**
     * Returns the 'void' literal.
     */
    private String getVoid() {
        return "void";
    }

    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    private static void cleansUpNesting() {
        try {
            final java.lang.reflect.Field field =
                Class.forName("org.eolang.PhDefault").getDeclaredField("NESTING");
            field.setAccessible(true);
            final ThreadLocal<?> nesting = (ThreadLocal<?>) field.get(null);
            nesting.remove();
        } catch (final ReflectiveOperationException ex) {
            throw new IllegalStateException(ex);
        }
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
                    self -> new Data.ToPhi(new SecureRandom().nextDouble())
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
            this.add("void", new AtVoid("void"));
            this.add("plus", new AtComposite(this, rho -> new PhDefault()));
            this.add(
                Phi.PHI,
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> rho.take("void")
                    )
                )
            );
            this.add(
                "context-hasContextedChildWithSetRhoWhenFormed",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> {
                            final Phi plus = new Data.ToPhi(5L).take(
                                "plus"
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
            this.add("kid", new AtComposite(this, rho -> new PhDefaultTest.Kid()));
            this.add("φ", new AtComposite(this, rho -> new Data.ToPhi(5L)));
        }
    }

    /**
     * Dummy.
     * @since 0.1.0
     */
    public static class WithVoidPhi extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        WithVoidPhi() {
            this.add(Phi.PHI, new AtVoid(Phi.PHI));
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
                Phi.PHI,
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
            this.add(Phi.PHI, new AtComposite(this, rho -> new Data.ToPhi(true)));
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
                Phi.PHI,
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
