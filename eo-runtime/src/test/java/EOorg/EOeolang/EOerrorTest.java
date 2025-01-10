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

import java.nio.ByteBuffer;
import java.util.stream.Stream;
import org.eolang.AtComposite;
import org.eolang.AtCompositeTest;
import org.eolang.AtOnce;
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
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link EOerror}.
 *
 * @since 0.26
 */
final class EOerrorTest {

    @Test
    void makesToxicObject() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(
                new PhWith(
                    new EOerror(),
                    "message",
                    new Data.ToPhi("intentional error")
                )
            ).take(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @ParameterizedTest
    @MethodSource("getTestSources")
    void getsReadableError(final byte[] cnst, final String text) {
        MatcherAssert.assertThat(
            "Bytes must be translated to string correctly",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(new MyError(cnst)).take()
            ).toString(),
            Matchers.containsString(text)
        );
    }

    /**
     * Static method providing sources for parameterized test.
     * @return Stream of sources.
     */
    private static Stream<Arguments> getTestSources() {
        return Stream.of(
            Arguments.of(
                ByteBuffer.allocate(Double.BYTES).putDouble(12.345_67D).array(),
                "12.34567"
            ),
            Arguments.of(new byte[]{1}, "[0x01] = true"),
            Arguments.of(new byte[]{0}, "[0x00] = false"),
            Arguments.of(new byte[]{}, "[<no bytes>]"),
            Arguments.of(new byte[]{12}, "[0x0C] = true"),
            Arguments.of(new byte[]{6, 5, 81, 99}, "[0x06055163-] = \"\\u0006\\u0005Qc\"")
        );
    }

    /**
     * The object below.
     * [] > my-error
     *   error > @
     *     "qwerty"
     * @since 0.35
     * @checkstyle JavadocStyleCheck
     */
    private static final class MyError extends PhDefault {

        /**
         * Ctor.
         * @param data The data inside error.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        MyError(final Object data) {
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> new PhWith(
                            new PhCopy(
                                Phi.Φ.take("org").take("eolang").take("error")
                            ),
                            "message",
                            new Data.ToPhi(data)
                        )
                    )
                )
            );
        }
    }

}
