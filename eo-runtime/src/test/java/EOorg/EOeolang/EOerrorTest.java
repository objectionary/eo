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
 */
package EOorg.EOeolang;

import java.util.stream.Stream;
import org.eolang.AtComposite;
import org.eolang.AtOnce;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhCopy;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.VerboseBytesAsStringTest;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
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
                    new EOerror(Phi.Φ),
                    "α",
                    new Data.ToPhi("intentional error")
                )
            ).take()
        );
    }

    @ParameterizedTest
    @MethodSource("getTestSources")
    void getsReadableError(final Object cnst) {
        ExAbstract error = null;
        try {
            new Dataized(new MyError(cnst)).take();
        } catch (final ExAbstract exc) {
            error = exc;
        }
        assert error != null;
        MatcherAssert.assertThat(
            error.toString(),
            Matchers.containsString(
                new VerboseBytesAsStringTest.ArgumentsUtils().toString(cnst)
            )
        );
    }

    /**
     * Input arguments for getsReadableError unit tests.
     * @return Stream of arguments.
     */
    private static Stream<Object> getTestSources() {
        return new VerboseBytesAsStringTest.ArgumentsUtils().getTestSources();
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
        MyError(final Object data) {
            this.add(
                "φ",
                new AtOnce(
                    new AtComposite(
                        this,
                        rho -> new PhWith(
                            new PhCopy(
                                Phi.Φ.attr("org").get().attr("eolang").get().attr("error").get()
                            ),
                            "α",
                            new Data.ToPhi(data)
                        )
                    )
                )
            );
        }
    }

}
