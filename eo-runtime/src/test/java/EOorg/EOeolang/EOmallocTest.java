/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtComposite;
import org.eolang.AtVoid;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOmalloc}.
 *
 * @since 0.1
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOmallocTest {
    @Test
    void freesMemory() {
        final Dummy dummy = new Dummy();
        new Dataized(EOmallocTest.allocated(new Data.ToPhi(1L), dummy)).take();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            "Heaps should throw an exception on attempt to free already freed memory, but it didn't"
        );
    }

    @Test
    void throwsOnErrorDummy() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                EOmallocTest.allocated(
                    new Data.ToPhi(1L),
                    new ErrorDummy()
                )
            ).take(),
            "Should throw an exception on attempting to use ErrorDummy, but it didn't"
        );
    }

    @Test
    void freesMemoryIfErrorIsOccurred() {
        final ErrorDummy dummy = new ErrorDummy();
        try {
            new Dataized(EOmallocTest.allocated(new Data.ToPhi(1L), dummy)).take();
        } catch (final ExAbstract ignored) {
        }
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
        final Phi malloc = Phi.Φ.take("org.eolang.malloc").take("for").copy();
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
                        return new PhWith(
                            new PhCopy(
                                Phi.Φ.take("org.eolang.error")
                            ),
                            0, new Data.ToPhi("Some failure")
                        );
                    }
                )
            );
        }
    }
}
