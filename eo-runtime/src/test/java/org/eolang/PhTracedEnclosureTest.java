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

import EOorg.EOeolang.EOcage;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhTracedEnclosure}.
 *
 * @since 0.24
 */
public class PhTracedEnclosureTest {

    @Test
    void throwsIfZeroDepth() {
        final PhTracedEnclosure traced = new PhTracedEnclosure(
            new Data.ToPhi(0L), new EOcage(Phi.Φ), 0
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(traced).take(),
            "We expect that dataization is not allowed if depth is less than 0"
        );
    }

    @Test
    void allowsSinglePass() {
        final long data = 0L;
        final PhTracedEnclosure traced = new PhTracedEnclosure(
            new Data.ToPhi(data), new EOcage(Phi.Φ), 1
        );
        MatcherAssert.assertThat(
            "Dataization of object without recursion is allowed if depth is more than 0",
            new Dataized(traced).take(Long.class),
            Matchers.equalTo(data)
        );
    }
}
