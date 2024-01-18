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
 * Test case for {@link AtConst}.
 *
 * @since 0.29.0
 */
class AtConstTest {

    @Test
    void convertsToString() {
        MatcherAssert.assertThat(
            new AtConst(new AtSimple(), Phi.Φ).toString(),
            Matchers.equalTo("ΦS!")
        );
    }

    @Test
    void convertsToTerm() {
        MatcherAssert.assertThat(
            new AtConst(new AtSimple(), Phi.Φ).φTerm(),
            Matchers.equalTo("Φ!")
        );
    }

    @Test
    void copies() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new AtConst(new AtSimple(), Phi.Φ).copy(Phi.Φ)
        );
    }

    @Test
    void puts() {
        final AtSimple simple = new AtSimple();
        new AtConst(simple, Phi.Φ).put(Phi.Φ);
        MatcherAssert.assertThat(
            Phi.Φ,
            Matchers.equalTo(simple.get())
        );
    }
}
