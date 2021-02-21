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
package org.eolang;

import org.eolang.phi.Phi;
import org.eolang.phi.Data;
import org.eolang.phi.Datarized;
import org.eolang.phi.PhMethod;
import org.eolang.phi.PhWith;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link EOint}.
 *
 * @since 0.1
 */
public final class EOintEOpowTest {

    @Test
    public void powersNumber() throws Exception {
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(new Data.ToPhi(2L), "pow"),
                    0,
                    new Data.ToPhi(4L)
                )
            ).take(Long.class),
            Matchers.equalTo(16L)
        );
    }

    @Test
    public void zeroToZeroPower() throws Exception {
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(new Data.ToPhi(0L), "pow"),
                    0,
                    new Data.ToPhi(0L)
                )
            ).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void zeroToOnePower() throws Exception {
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(new Data.ToPhi(0L), "pow"),
                    0,
                    new Data.ToPhi(1L)
                )
            ).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    public void zeroToTwoPower() throws Exception {
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(new Data.ToPhi(0L), "pow"),
                    0,
                    new Data.ToPhi(2L)
                )
            ).take(Long.class),
            Matchers.equalTo(0L)
        );
    }

    @Test
    public void zeroToNegativePowerFails() throws Exception {
        final Phi result = new PhWith(
            new PhMethod(new Data.ToPhi(0L), "pow"),
            0,
            new Data.ToPhi(-1L)
        );
        MatcherAssert.assertThat(
            new Datarized(
                result.attr("msg").get()
            ).take(String.class),
            Matchers.equalTo("0 cannot be raised to a negative power")
        );
        Assertions.assertThrows(org.eolang.phi.Attr.Exception.class, () -> {
            new Datarized(result).take(Long.class);
        });
    }

}
