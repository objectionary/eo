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

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.number.SumOf;
import org.eolang.AtCompositeTest;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhTraced;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test cases for {@link EOcage}.
 * @since 0.19
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOcageTest {
    @Test
    void encagesViaApplication() {
        final Phi cage = EOcageTest.encaged(new Data.ToPhi(1));
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(cage).asNumber(),
            Matchers.equalTo(1.0)
        );
    }

    @Test
    void encagesAndFrees() {
        final Phi cage = EOcageTest.encaged(new Data.ToPhi(1));
        EOcageTest.encageTo(cage, new Data.ToPhi(2));
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(cage).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @Test
    void overwritesCagedObject() {
        final Phi cage = EOcageTest.encaged(
            new PhWith(
                new EOcageTest.Dummy(),
                0, new Data.ToPhi(1)
            )
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(new PhMethod(cage, "x")).asNumber(),
            Matchers.equalTo(1.0)
        );
        EOcageTest.encageTo(
            cage,
            new PhWith(
                new EOcageTest.Dummy(),
                0, new Data.ToPhi(2)
            )
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(new PhMethod(cage, "x")).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @Test
    void encagesObjectOnCopy() {
        final Phi first = EOcageTest.encaged(new Data.ToPhi(1L));
        final Phi second = first.copy();
        EOcageTest.encageTo(second, new Data.ToPhi(2L));
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(first).asNumber(),
            Matchers.equalTo(2.0)
        );
    }

    @Test
    void writesAndRewritesPrimitive() {
        final Phi cage = EOcageTest.encaged(new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(cage).asNumber(),
            Matchers.equalTo(1.0)
        );
        EOcageTest.encageTo(cage, new Data.ToPhi(5L));
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(cage).asNumber(),
            Matchers.equalTo(5.0)
        );
    }

    @Test
    void doesNotWritePrimitivesFormedDifferently() {
        final Phi cage = EOcageTest.encaged(new Data.ToPhi(1L));
        Assertions.assertThrows(
            ExAbstract.class,
            () -> EOcageTest.encageTo(cage, new Data.ToPhi("Hello world")),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void doesNotWriteBoundedMethod() {
        final Phi five = new Data.ToPhi(5L);
        final Phi ten = new PhWith(
            five.take("plus").copy(),
            "x",
            new Data.ToPhi(5L)
        );
        final Phi cage = EOcageTest.encaged(five);
        Assertions.assertThrows(
            ExAbstract.class,
            () -> EOcageTest.encageTo(cage, ten),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void writesBoundedCopyOfTheSameBase() {
        final Phi dummy = new Dummy();
        Assertions.assertDoesNotThrow(
            () -> EOcageTest.encageTo(
                EOcageTest.encaged(dummy),
                new PhWith(new PhCopy(dummy), "x", new Data.ToPhi("Hello world"))
            ),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    /**
     * Encage given object to given cage.
     * @param cage Cage
     * @param obj Object
     */
    private static void encageTo(final Phi cage, final Phi obj) {
        final Phi write = cage.take("encage").copy();
        write.put(0, obj);
        new Dataized(write).take();
    }

    /**
     * Encaged object.
     * @param obj Object to put
     * @return Cage.new object
     */
    private static Phi encaged(final Phi obj) {
        final Phi cage = Phi.Φ.take("org.eolang.cage").copy();
        cage.put(0, obj);
        return cage.take("new");
    }

    /**
     * Cases to test behaviour of cage with recursion.
     * @since 0.1
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    final class RecursionTests {

        /**
         * DEPTH.
         */
        private static final int MAX_DEPTH = 50;

        @BeforeEach
        void setDepth() {
            System.setProperty(
                PhTraced.RECURSION_LIMIT, String.valueOf(EOcageTest.RecursionTests.MAX_DEPTH)
            );
        }

        @AfterEach
        void clearDepth() {
            System.clearProperty(PhTraced.RECURSION_LIMIT);
        }

        @Test
        void doesNotThrowIfDataizesConcurrently() {
            final Phi cage = EOcageTest.encaged(new RecursiveDummy());
            EOcageTest.encageTo(
                cage,
                new RecursiveDummy(EOcageTest.RecursionTests.MAX_DEPTH, cage)
            );
            final int threads = 500;
            MatcherAssert.assertThat(
                AtCompositeTest.TO_ADD_MESSAGE,
                new SumOf(
                    new Threads<>(
                        threads,
                        Stream.generate(
                            () -> (Scalar<Integer>) () -> {
                                new Dataized(cage).take();
                                return 1;
                            }
                        ).limit(threads).collect(Collectors.toList())
                    )
                ).intValue(),
                Matchers.equalTo(threads)
            );
        }

        // [] > test
        //   [x] > dummy
        //     x > @
        //   cage.new (dummy 1) > c
        //   seq > @
        //     *
        //       c.write (dummy c')
        //       c.x.x.x # fail
        @Test
        void rewritesItselfToItselfViaDummy() {
            System.setProperty(
                PhTraced.RECURSION_LIMIT, "2"
            );
            final Phi cage = EOcageTest.encaged(
                new PhWith(new EOcageTest.Dummy(), 0, new Data.ToPhi(1L))
            );
            EOcageTest.encageTo(cage, new PhWith(new EOcageTest.Dummy(), 0, cage.copy()));
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    cage.take("x").take("x").take("x")
                ).asNumber()
            );
        }

        @Test
        void throwsExceptionIfRecursion() {
            final Phi cage = EOcageTest.encaged(
                Phi.Φ.take("org.eolang.cage").take("encaged")
            );
            EOcageTest.encageTo(cage, cage);
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(cage).take(),
                "We expect the exception to be thrown since we have recursion here"
            );
        }

        @Test
        void doesNotThrowExceptionIfSmallDepth() {
            final Phi cage = EOcageTest.encaged(new RecursiveDummy());
            EOcageTest.encageTo(
                cage,
                new RecursiveDummy(EOcageTest.RecursionTests.MAX_DEPTH / 2, cage)
            );
            Assertions.assertDoesNotThrow(
                () -> new Dataized(cage).take(),
                String.format(
                    "We expect that dataizing of nested cage which recursion depth is less than property %s = %s does not throw %s",
                    PhTraced.RECURSION_LIMIT,
                    System.getProperty(PhTraced.RECURSION_LIMIT),
                    ExAbstract.class
                )
            );
        }

        /**
         * The boundary case when the depth is equal to the maximum allowed.
         */
        @Test
        void doesNotThrowExceptionIfMaxDepth() {
            final Phi cage = EOcageTest.encaged(new RecursiveDummy());
            EOcageTest.encageTo(
                cage,
                new RecursiveDummy(EOcageTest.RecursionTests.MAX_DEPTH, cage)
            );
            Assertions.assertDoesNotThrow(
                () -> new Dataized(cage).take(),
                String.format(
                    "We expect that dataizing of nested cage which recursion depth is equal to property %s = %s does not throw %s",
                    PhTraced.RECURSION_LIMIT,
                    System.getProperty(PhTraced.RECURSION_LIMIT),
                    ExAbstract.class
                )
            );
        }

        @Test
        void throwsExceptionIfBigDepth() {
            final Phi cage = EOcageTest.encaged(new RecursiveDummy());
            EOcageTest.encageTo(
                cage,
                new RecursiveDummy(EOcageTest.RecursionTests.MAX_DEPTH + 1, cage)
            );
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(cage).take(),
                String.format(
                    "We expect that dataizing of nested cage which recursion depth is more than property %s = %s does not throw %s",
                    PhTraced.RECURSION_LIMIT,
                    System.getProperty(PhTraced.RECURSION_LIMIT),
                    ExAbstract.class
                )
            );
        }

        /**
         * Recursive {@link Phi}.
         * @since 0.1
         */
        private final class RecursiveDummy extends PhDefault implements Atom {

            /**
             * How many times should we met the cage while dataizing it eventually.
             */
            private final Integer depth;

            /**
             * The cage.
             */
            private final Phi cage;

            /**
             * Counts how many times we already met the cage.
             */
            private final ThreadLocal<Integer> counter;

            /**
             * Ctor for initialize cage.
             */
            RecursiveDummy() {
                this(null, null);
            }

            /**
             * Ctor.
             * @param depth Depth.
             * @param cage Cage.
             */
            RecursiveDummy(final Integer depth, final Phi cage) {
                this.depth = depth;
                this.cage = cage;
                this.counter = ThreadLocal.withInitial(() -> 0);
            }

            @Override
            public Phi lambda() {
                final Phi ret;
                this.counter.set(this.counter.get() + 1);
                if (this.counter.get() >= this.depth) {
                    ret = new Data.ToPhi(0L);
                } else {
                    ret = this.cage;
                }
                return ret;
            }
        }
    }

    /**
     * Dummy Phi.
     * @since 1.0
     */
    private static final class Dummy extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Dummy() {
            this.add("x", new AtVoid("x"));
        }
    }
}
