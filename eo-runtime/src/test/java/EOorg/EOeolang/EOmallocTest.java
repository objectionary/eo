/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import org.eolang.AtComposite;
import org.eolang.AtCompositeTest;
import org.eolang.AtVoid;
import org.eolang.Attr;
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
        final Phi phi = EOmallocTest.allocated(
            new Data.ToPhi(1L),
            dummy
        );
        new Dataized(phi).take();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void freesMemoryIfErrorIsOccurred() {
        final ErrorDummy dummy = new ErrorDummy();
        final Phi phi = EOmallocTest.allocated(
            new Data.ToPhi(1L),
            dummy
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(phi).take(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            AtCompositeTest.TO_ADD_MESSAGE
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
            this.add("m", new AtVoid("m"));
            this.add(
                Attr.PHI,
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
            this.add("m", new AtVoid("m"));
            this.add(
                Attr.PHI,
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
