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
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOmalloc$EOof$EOallocated$EOread}.
 *
 * @since 0.51.2
 * @checkstyle TypeNameCheck (5 lines)
 */
@SuppressWarnings("PMD.AvoidDollarSigns")
final class EOmalloc$EOof$EOallocated$EOreadTest {

    @Test
    void throwsCorrectErrorForOffsetAttrNaN() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new Dummy().it(),
                        "offset",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "put TRUE in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForOffsetAttrNotAnInt() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new Dummy().it(),
                        "offset",
                        new Data.ToPhi(42.42)
                    )
                ).take(),
                "put double in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'offset' attribute (42.42) must be an integer")
        );
    }

    @Test
    void throwsCorrectErrorForLengthAttrNaN() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhWith(
                            new Dummy().it(),
                            "offset",
                            new Data.ToPhi(42)
                        ),
                        "length",
                        new Data.ToPhi(true)
                    )
                ).take(),
                "put TRUE in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'length' attribute must be a number")
        );
    }

    @Test
    void throwsCorrectErrorForLengthAttrNotAnInt() {
        MatcherAssert.assertThat(
            "the message in the error is correct",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhWith(
                        new PhWith(
                            new Dummy().it(),
                            "offset",
                            new Data.ToPhi(42)
                        ),
                        "length",
                        new Data.ToPhi(42.42)
                    )
                ).take(),
                "put double in int attr fails with a proper message that explains what happened"
            ).getMessage(),
            Matchers.equalTo("the 'length' attribute (42.42) must be an integer")
        );
    }

    /**
     * Dummy with correct id attr.
     * @since 0.51.2
     */
    @SuppressWarnings("PMD.MethodNameCheck")
    private static final class Dummy {
        /**
         * Produce EOmalloc$EOof$EOallocated$EOread with id attr.
         * @checkstyle TypeNameCheck (5 lines)
         */
        private Phi it() {
            return new PhWith(
                new EOmalloc$EOof$EOallocated$EOread(),
                Attr.RHO,
                new PhWith(
                    new EOmallocTest.IdDummy(),
                    "id",
                    new Data.ToPhi(42)
                )
            );
        }
    }

}
