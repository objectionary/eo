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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

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
            AtCompositeTest.TO_ADD_MESSAGE
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
            AtCompositeTest.TO_ADD_MESSAGE
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> Heaps.INSTANCE.free((int) dummy.id),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOmalloc$EOof$EOallocated$EOread.class,
        EOmalloc$EOof$EOallocated$EOresized.class,
        EOmalloc$EOof$EOallocated$EOsize.class,
        EOmalloc$EOof$EOallocated$EOwrite.class
    })
    void throwsCorrectErrorForIdAttrNaN(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        (Phi) cls.getDeclaredConstructor().newInstance(),
                        Attr.RHO,
                        new PhWith(
                            new EOmallocTest.IdDummy(),
                            "id",
                            new Data.ToPhi(true)
                        )
                    )
                ).take(),
                "put TRUE in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'id' attribute must be a number")
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOmalloc$EOof$EOallocated$EOread.class,
        EOmalloc$EOof$EOallocated$EOresized.class,
        EOmalloc$EOof$EOallocated$EOsize.class,
        EOmalloc$EOof$EOallocated$EOwrite.class
    })
    void throwsCorrectErrorForIdAttrNotAnInt(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        (Phi) cls.getDeclaredConstructor().newInstance(),
                        Attr.RHO,
                        new PhWith(
                            new EOmallocTest.IdDummy(),
                            "id",
                            new Data.ToPhi(42.42)
                        )
                    )
                ).take(),
                "put double in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'id' attribute (42.42) must be an integer")
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOmalloc$EOof$EOallocated$EOread.class,
        EOmalloc$EOof$EOallocated$EOwrite.class
    })
    void throwsCorrectErrorForOffsetAttrNaN(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhiWithIdDummy(
                            (Phi) cls.getDeclaredConstructor().newInstance()
                        ).it(),
                        "offset",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "put TRUE in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute must be a number")
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOmalloc$EOof$EOallocated$EOread.class,
        EOmalloc$EOof$EOallocated$EOwrite.class
    })
    void throwsCorrectErrorForOffsetAttrNotAnInt(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhiWithIdDummy(
                            (Phi) cls.getDeclaredConstructor().newInstance()
                        ).it(),
                        "offset",
                        new Data.ToPhi(42.42)
                    )
                ).take(),
                "put double in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute (42.42) must be an integer")
        );
    }

    @ParameterizedTest
    @ValueSource(classes = {
        EOmalloc$EOof$EOallocated$EOread.class,
        EOmalloc$EOof$EOallocated$EOwrite.class
    })
    void throwsCorrectErrorForOffsetAttrNotNatural(final Class<?> cls) {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhiWithIdDummy(
                            (Phi) cls.getDeclaredConstructor().newInstance()
                        ).it(),
                        "offset",
                        new Data.ToPhi(-42)
                    )
                ).take(),
                "put negative int in natural attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute (-42) must be greater or equal to zero")
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
     * Phi with Dummy with 'id' attribute.
     *
     * @since 0.52
     */
    @SuppressWarnings("PMD.ShortMethodName")
    static class PhiWithIdDummy {
        /**
         * Phi.
         */
        private final Phi phi;

        /**
         * Ctor.
         * @param phi Phi
         */
        PhiWithIdDummy(final Phi phi) {
            this.phi = phi;
        }

        /**
         * Return it.
         * @checkstyle MethodNameCheck (5 lines)
         */
        public Phi it() {
            return new PhWith(
                this.phi,
                Attr.RHO,
                new PhWith(
                    new EOmallocTest.IdDummy(),
                    "id",
                    new Data.ToPhi(42)
                )
            );
        }
    }

    /**
     * Dummy with id attr.
     *
     * @since 0.52
     */
    private static class IdDummy extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        IdDummy() {
            this.add("id", new AtVoid("id"));
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
