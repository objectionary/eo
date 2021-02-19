/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
package org.eolang.txt;

import org.eolang.phi.Data;
import org.eolang.phi.Datarized;
import org.eolang.phi.PhMethod;
import org.eolang.phi.PhWith;
import org.eolang.phi.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOregex$EOmatch}.
 *
 * @since 0.1
 */
public final class EOregex$EOmatchTest {

    @Test
    public void matchesString() {
        final Phi regex = new PhWith(
            new org.eolang.txt.EOregex(),
            "pattern",
            new Data.ToPhi("([a-z]+)")
        );
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(regex, "match"),
                    "txt",
                    new Data.ToPhi("hello")
                )
            ).take(Phi[].class).length,
            Matchers.equalTo(1)
        );
    }

    @Test
    public void doesntMatchString() {
        final Phi regex = new PhWith(
            new org.eolang.txt.EOregex(),
            "pattern",
            new Data.ToPhi("([A-Z]{2})")
        );
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(regex, "match"),
                    "txt",
                    new Data.ToPhi("Hello, World!")
                )
            ).take(Phi[].class).length,
            Matchers.equalTo(0)
        );
    }

}
