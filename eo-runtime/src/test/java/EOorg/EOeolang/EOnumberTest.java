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

import EOorg.EOeolang.EOio.EOconsole$EOwrite$EOwritten_bytes;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.eolang.AtComposite;
import org.eolang.AtCompositeTest;
import org.eolang.AtOnce;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link EOnumber}.
 *
 * @since 0.39
 * @checkstyle TypeNameCheck (4 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOnumberTest {

    @Test
    void hasDifferentHashes() {
        final Phi left = new Data.ToPhi(42L);
        final Phi right = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            left.hashCode(),
            Matchers.not(Matchers.equalTo(right.hashCode()))
        );
    }

    @Test
    void hasHashEvenWithoutData() {
        final Phi phi = new EOnumber();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            phi.hashCode(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void hasDifferentHash() {
        final Phi raw = new EOnumber();
        final Phi initialized = new Data.ToPhi(0L);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            raw.hashCode(),
            Matchers.not(initialized.hashCode())
        );
    }

    @ParameterizedTest()
    @CsvSource({"lt", "gt", "lte", "gte"})
    void doesNotDataizeArgumentTwiceComparisonMethods(final String method) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final PrintStream out = new PrintStream(baos);
        final String str = "Hello world";
        new Dataized(
            new EOnumberTest.PrintWithCmp(
                new PhMethod(
                    new Data.ToPhi(1.0),
                    method
                ),
                new Data.ToPhi(3.0),
                new PhWith(
                    new EOconsole$EOwrite$EOwritten_bytes(out),
                    "buffer",
                    new Data.ToPhi(str)
                )
            )
        ).take();
        MatcherAssert.assertThat(
            String.format(
                "The object `number.%s` should have not dataize its argument twice, but it did",
                method
            ),
            baos.toString(),
            Matchers.equalTo(str)
        );
        out.close();
    }

    /**
     * PrintWithCmp Phi.
     *
     * @since 1.0
     */
    private static class PrintWithCmp extends PhDefault {
        /**
         * Ctor.
         * E.g.
         * 1.lt
         *   seq
         *     *
         *       stdout "Hello world"
         *       3
         *
         * @param method Comparison PhMethod ("lt", "gt", "lte", "gte")
         * @param value Phi value to be compared
         * @param stdout Object that can print
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PrintWithCmp(final Phi method, final Phi value, final Phi stdout) {
            super();
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        self -> new ToPhi(
                            new Dataized(
                                new PhWith(
                                    method,
                                    0,
                                    new PhWith(
                                        new EOseq(),
                                        0,
                                        new PhWith(
                                            new PhWith(
                                                new EOtuple$EOempty()
                                                    .take("with")
                                                    .copy(),
                                                0,
                                                stdout
                                            ).take("with").copy(),
                                            0, value
                                        )
                                    )
                                )
                            ).take()
                        )
                    )
                )
            );
        }
    }
}
