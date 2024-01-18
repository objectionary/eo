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
package org.eolang;

import EOorg.EOeolang.EOerror;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtNamed}.
 *
 * @since 0.21
 */
final class AtNamedTest {

    @Test
    void rethrowsCorrectly() {
        final Phi phi = new AtNamedTest.Dummy();
        final EOerror.ExError error = Assertions.assertThrows(
            EOerror.ExError.class,
            () -> phi.attr("x").get().attr("anything").get()
        );
        MatcherAssert.assertThat(
            new Dataized(error.enclosure()).take(String.class),
            Matchers.allOf(
                Matchers.containsString(
                    "Error at \"org.eolang.AtNamedTest.Dummy#x\" attribute"
                ),
                Matchers.containsString("caused by intended")
            )
        );
    }

    /**
     * Dummy Phi.
     * @since 1.0
     */
    private static class Dummy extends PhDefault {

        /**
         * Ctor.
         */
        Dummy() {
            super();
            this.add(
                "x",
                new AtComposite(
                    this,
                    rho -> {
                        throw new ExFailure("intended");
                    }
                )
            );
        }
    }

}
