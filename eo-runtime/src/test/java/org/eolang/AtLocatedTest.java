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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtLocated}.
 *
 * @since 0.29.0
 */
class AtLocatedTest {

    @Test
    void convertsToString() {
        MatcherAssert.assertThat(
            new AtLocated(new AtSimple(), 10, 10).toString(),
            Matchers.equalTo("<?:10:10>ΦS")
        );
    }

    @Test
    void getsPhiTermFromOrigin() {
        final AtSimple origin = new AtSimple();
        MatcherAssert.assertThat(
            new AtLocated(origin, 10, 10).φTerm(),
            Matchers.equalTo(origin.φTerm())
        );
    }

    @Test
    void copies() {
        final AtLocated origin = new AtLocated(new AtSimple(), 10, 10);
        MatcherAssert.assertThat(
            origin.copy(Phi.Φ).toString(),
            Matchers.equalTo(origin.toString())
        );
    }

    @Test
    void rethrowsExFlowException() {
        Assertions.assertThrows(
            ExFlow.class,
            () -> new AtLocated(new AtFailed(), 10, 10).get()
        );
    }

    @Test
    void rethrowsExFailure() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new AtLocated(new AtFailed(new ExUnset("Unset")), 10, 10).get()
        );
    }

    @Test
    void putsWithExFlowException() {
        Assertions.assertThrows(
            ExFlow.class,
            () -> new AtLocated(new AtFailed(), 10, 10).put(Phi.Φ)
        );
    }

    /**
     * Attribute that trows an exception on all methods.
     *
     * @since 0.29.0
     */
    private static final class AtFailed implements Attr {

        /**
         * Exception to throw.
         */
        private final ExAbstract exception;

        /**
         * Ctor.
         */
        private AtFailed() {
            this(new ExFlow());
        }

        /**
         * Ctor.
         * @param except Exception to throw.
         */
        private AtFailed(final ExAbstract except) {
            this.exception = except;
        }

        @Override
        public Attr copy(final Phi self) {
            throw this.exception;
        }

        @Override
        public Phi get() {
            throw this.exception;
        }

        @Override
        public void put(final Phi phi) {
            throw this.exception;
        }

        @Override
        public String φTerm() {
            throw this.exception;
        }
    }
}
