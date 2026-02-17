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
@SuppressWarnings("PMD.TooManyMethods")
final class PhDefaultTest {

    @Test
    void comparesTwoObjects() {
        final Phi phi = PhDefaultTest.Int.make();
        MatcherAssert.assertThat(
            "Object should be equal to itself",
            phi, Matchers.equalTo(phi)
        );
    }

    @Test
    void copiesSetVoidAttributeOnCopy() {
        final Phi phi = PhDefaultTest.Int.make();
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
        final Phi phi = PhDefaultTest.Int.make();
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
    void throwsWhenAccessingPhiWithoutVoidSet() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new PhSafe(PhDefaultTest.Int.make().copy()).take(Phi.PHI),
            "Phi should not be accessible without setting void attribute, but it did"
        );
    }

    @Test
    void accessesPhiAfterSettingVoid() {
        final Phi phi = new PhSafe(PhDefaultTest.Int.make().copy());
        phi.put(this.getVoid(), new Data.ToPhi(10L));
        Assertions.assertDoesNotThrow(
            () -> phi.take(Phi.PHI),
            "Phi should be accessible after setting void attribute, but it didn't"
        );
    }

    @Test
    void hasContextedChildWithSetRhoWhenFormed() {
        final Phi phi = PhDefaultTest.Int.make();
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
        MatcherAssert.assertThat(
            "Object should have a hashCode greater then 0, but it didn't",
            PhDefaultTest.Int.make().hashCode(),
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
                    t -> Int.make()
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
        final Phi phi = PhDefaultTest.Int.make();
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
        final Phi phi = PhDefaultTest.Foo.make();
        phi.put(0, new Data.ToPhi(42L));
        Assertions.assertThrows(
            ExReadOnly.class,
            () -> phi.put(0, new Data.ToPhi(42L)),
            "Setting void attribute more than once should fail, but it didn't"
        );
    }

    @Test
    void printsEndlessRecursionObject() {
        MatcherAssert.assertThat(
            "Dataization should discover the infinite recursion, but it didn't",
            new Dataized(EndlessRecursion.make(2)).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void hesPhiRecursively() {
        MatcherAssert.assertThat(
            "Dataization should discover the infinite recursion, but it didn't",
            new Dataized(RecursivePhi.make(3)).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void cachesPhiViaNewRecursively() {
        MatcherAssert.assertThat(
            "Does not cache phi via new recursively",
            new Dataized(RecursivePhiViaNew.make(3)).asNumber(),
            Matchers.equalTo(0.0)
        );
    }

    @Test
    void doesNotReadMultipleTimes() {
        final Phi phi = PhDefaultTest.Counter.make();
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
        final Phi phi = WithVoidPhi.make();
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
                        Rnd.make(), this.plus()
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
    void failsWhenTooManyAttributesPut() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new EOnumber().put(1, new Data.ToPhi(1)),
            "Should fail when setting attribute with too big position, but it didn't"
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
    void throwsForNonExistentAttribute() {
        Assertions.assertThrows(
            ExUnset.class,
            () -> this.phiWithContextAttribute(
                "context-throwsForNonExistentAttribute"
            ).take("non-existent-attribute"),
            "Should throw exception for non-existent attribute, but it didn't"
        );
    }

    @Test
    void worksAfterException() {
        final Phi phi = this.phiWithContextAttribute("context-worksAfterException");
        try {
            phi.take("non-existent-attribute");
        } catch (final ExUnset ignored) {
        }
        Assertions.assertDoesNotThrow(
            () -> phi.take("context-worksAfterException"),
            "Should still work after exception, but it didn't"
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
                            phi.forma();
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
    private static final class Rnd extends PhDefault {
        /**
         * Factory method.
         * @return New Rnd instance
         */
        static Rnd make() {
            final Rnd rnd = new Rnd();
            rnd.add(
                "φ",
                new AtComposite(
                    rnd,
                    self -> new Data.ToPhi(new SecureRandom().nextDouble())
                )
            );
            return rnd;
        }
    }

    /**
     * Int.
     * @since 0.36.0
     */
    static final class Int extends PhDefault {
        /**
         * Factory method.
         * @return New Int instance
         */
        static Int make() {
            final Int obj = new Int();
            obj.add("void", new AtVoid("void"));
            obj.add("plus", new AtComposite(obj, rho -> new PhDefault()));
            obj.add(
                Phi.PHI,
                new AtOnce(
                    new AtComposite(
                        obj,
                        rho -> rho.take("void")
                    )
                )
            );
            obj.add(
                "context-hasContextedChildWithSetRhoWhenFormed",
                new AtOnce(
                    new AtComposite(
                        obj,
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
            return obj;
        }
    }

    /**
     * Foo.
     * @since 0.1.0
     */
    public static final class Foo extends PhDefault {
        /**
         * Factory method.
         * @return New Foo instance
         */
        static Foo make() {
            final Foo foo = new Foo();
            foo.add("x", new AtVoid("x"));
            foo.add("kid", new AtComposite(foo, rho -> PhDefaultTest.Kid.make()));
            foo.add("φ", new AtComposite(foo, rho -> new Data.ToPhi(5L)));
            return foo;
        }
    }

    /**
     * Dummy.
     * @since 0.1.0
     */
    public static final class WithVoidPhi extends PhDefault {
        /**
         * Factory method.
         * @return New WithVoidPhi instance
         */
        static WithVoidPhi make() {
            final WithVoidPhi obj = new WithVoidPhi();
            obj.add(Phi.PHI, new AtVoid(Phi.PHI));
            return obj;
        }
    }

    /**
     * Counter.
     * @since 0.1.0
     */
    public static final class Counter extends PhDefault {
        /**
         * Count.
         */
        private long count;

        /**
         * Factory method.
         * @return New Counter instance
         */
        static Counter make() {
            final Counter counter = new Counter();
            counter.add(
                Phi.PHI,
                new AtOnce(
                    new AtComposite(
                        counter,
                        rho -> {
                            ++counter.count;
                            return new Data.ToPhi(new byte[]{(byte) 0x01});
                        }
                    )
                )
            );
            counter.add("count", new AtComposite(counter, rho -> new Data.ToPhi(counter.count)));
            return counter;
        }
    }

    /**
     * Kid.
     * @since 0.1.0
     */
    public static final class Kid extends PhDefault {
        /**
         * Factory method.
         * @return New Kid instance
         */
        static Kid make() {
            final Kid kid = new Kid();
            kid.add("z", new AtVoid("z"));
            kid.add(Phi.PHI, new AtComposite(kid, rho -> new Data.ToPhi(true)));
            return kid;
        }
    }

    /**
     * Endless Recursion.
     * @since 0.1.0
     */
    public static final class EndlessRecursion extends PhDefault {
        /**
         * Factory method.
         * @param remaining Remaining iterations
         * @return New EndlessRecursion instance
         */
        static EndlessRecursion make(final int remaining) {
            final EndlessRecursion obj = new EndlessRecursion();
            obj.add(
                Phi.PHI,
                new AtComposite(
                    obj,
                    self -> {
                        final Phi result;
                        if (remaining <= 1) {
                            result = new Data.ToPhi(0L);
                        } else {
                            result = new PhCopy(EndlessRecursion.make(remaining - 1));
                        }
                        return result;
                    }
                )
            );
            return obj;
        }
    }

    /**
     * Recursive Phi.
     * @since 0.1.0
     */
    public static final class RecursivePhi extends PhDefault {
        /**
         * Factory method.
         * @param remaining Remaining iterations
         * @return New RecursivePhi instance
         */
        static RecursivePhi make(final int remaining) {
            final RecursivePhi obj = new RecursivePhi();
            obj.add(
                "φ",
                new AtComposite(
                    obj,
                    rho -> {
                        final Phi result;
                        if (remaining <= 1) {
                            result = new Data.ToPhi(0L);
                        } else {
                            result = new Data.ToPhi(
                                new Dataized(RecursivePhi.make(remaining - 1)).asNumber()
                            );
                        }
                        return result;
                    }
                )
            );
            return obj;
        }
    }

    /**
     * RecursivePhiViaNew.
     * @since 0.1.0
     */
    public static final class RecursivePhiViaNew extends PhDefault {
        /**
         * Factory method.
         * @param remaining Remaining iterations
         * @return New RecursivePhiViaNew instance
         */
        static RecursivePhiViaNew make(final int remaining) {
            final RecursivePhiViaNew obj = new RecursivePhiViaNew();
            obj.add(
                "φ",
                new AtComposite(
                    obj,
                    rho -> {
                        final Phi result;
                        if (remaining <= 1) {
                            result = new Data.ToPhi(0L);
                        } else {
                            result = new Data.ToPhi(
                                new Dataized(
                                    RecursivePhiViaNew.make(remaining - 1)
                                ).asNumber()
                            );
                        }
                        return result;
                    }
                )
            );
            return obj;
        }
    }
}
