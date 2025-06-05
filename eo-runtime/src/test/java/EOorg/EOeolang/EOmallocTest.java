/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhComposite;
import org.eolang.PhCompositeTest;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
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
        new Dataized(
            EOmallocTest.allocated(
                new Data.ToPhi(1L),
                dummy
            )
        ).take();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            PhCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void freesMemoryIfErrorIsOccurred() {
        final ErrorDummy dummy = new ErrorDummy();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(
                EOmallocTest.allocated(
                    new Data.ToPhi(1L),
                    dummy
                )
            ).take(),
            PhCompositeTest.TO_ADD_MESSAGE
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            PhCompositeTest.TO_ADD_MESSAGE
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
    private static class Dummy extends PhDefault {
        /**
         * Id.
         */
        private double id;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Dummy() {
            this.add("m", new PhVoid("m"));
            this.add(
                Phi.PHI,
                new PhComposite(
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
    private static class ErrorDummy extends PhDefault {
        /**
         * Id.
         */
        private double id;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        ErrorDummy() {
            this.add("m", new PhVoid("m"));
            this.add(
                Phi.PHI,
                new PhComposite(
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
