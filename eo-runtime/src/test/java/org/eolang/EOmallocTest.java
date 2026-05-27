/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOmalloc}.
 * @since 0.1
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOmallocTest {

    @Test
    void freesMemory() {
        final EOmallocTest.Dummy dummy = new EOmallocTest.Dummy();
        new Dataized(
            EOmallocTest.allocated(
                new Data.ToPhi(1L),
                dummy
            )
        ).take();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            "Heaps should throw an exception on attempt to free already freed memory, but it didn't"
        );
    }

    @Test
    void freesMemoryIfErrorIsOccurred() {
        final EOmallocTest.ErrorDummy dummy = new EOmallocTest.ErrorDummy();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                EOmallocTest.allocated(
                    new Data.ToPhi(1L),
                    dummy
                )
            ).take(),
            "Should throw an exception on attempting to use ErrorDummy, but it didn't"
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            "Heaps should throw an exception on attempting to free already freed memory after failure, but it didn't"
        );
    }

    /**
     * Allocated data.
     * @param obj Init object
     * @param dummy Dummy as scope
     * @return Malloc object
     */
    private static Phi allocated(final Phi obj, final Phi dummy) {
        final Phi malloc = Phi.Φ.take("malloc").take("for").copy();
        malloc.put(0, obj);
        malloc.put(1, dummy);
        return malloc;
    }

    /**
     * Dummy.
     * @since 0.37.0
     */
    private static final class Dummy extends PhDefault {

        /**
         * Id.
         */
        private double id;

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (20 lines)
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Dummy() {
            this.add("m", new AtVoid("m"));
            this.add(
                Phi.PHI,
                new AtComposite(
                    this,
                    rho -> {
                        this.id = new Dataized(
                            rho.take("m").take("id")
                        ).asNumber();
                        return new Data.ToPhi(true);
                    }
                )
            );
        }
    }

    /**
     * Dummy that throws an exception.
     * @since 0.36.0
     */
    private static final class ErrorDummy extends PhDefault {

        /**
         * Id.
         */
        private double id;

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (25 lines)
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        ErrorDummy() {
            this.add("m", new AtVoid("m"));
            this.add(
                Phi.PHI,
                new AtComposite(
                    this,
                    rho -> {
                        this.id = new Dataized(
                            this.take("m").take("id")
                        ).asNumber();
                        return new PhApplication(
                            Phi.Φ.take("error").copy(),
                            0, new Data.ToPhi("Some failure")
                        );
                    }
                )
            );
        }
    }
}
