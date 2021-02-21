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

import org.eolang.phi.Data;
import org.eolang.phi.Datarized;
import org.eolang.phi.PhCopy;
import org.eolang.phi.PhMethod;
import org.eolang.phi.PhWith;
import org.eolang.phi.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOmemory}.
 *
 * @since 0.1
 */
public final class EOmemoryTest {

    @Test
    public void readsAndWrites() throws Exception {
        final Phi mem = new org.eolang.EOmemory();
        final Phi text = new Data.ToPhi("Hello, world!");
        final Phi write = mem.attr("write").get();
        write.attr(0).put(text);
        new Datarized(write).take(Boolean.class);
        MatcherAssert.assertThat(
            new Datarized(mem).take(String.class),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    public void comparesForEquality() throws Exception {
        final Phi mem = new org.eolang.EOmemory();
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0, new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Datarized(
                new PhWith(
                    new PhMethod(mem, "eq"),
                    0,
                    new Data.ToPhi(1L)
                )
            ).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    public void writesAndRewrites() throws Exception {
        final Phi mem = new org.eolang.EOmemory();
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0, new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0, new Data.ToPhi(5L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Datarized(mem).take(Long.class),
            Matchers.equalTo(5L)
        );
    }

    @Test
    public void makeCorrectCopy() throws Exception {
        final Phi mem = new org.eolang.EOmemory();
        final Phi text = new Data.ToPhi(1L);
        final Phi write = mem.attr("write").get();
        write.attr(0).put(text);
        new Datarized(write).take(Boolean.class);
        MatcherAssert.assertThat(
            new Datarized(new PhCopy(mem)).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void comparesOnFly() throws Exception {
        final Phi mem = new org.eolang.EOmemory();
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0, new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        final Phi less = new PhWith(
            new PhMethod(mem, "less"),
            0,
            new Data.ToPhi(10L)
        );
        MatcherAssert.assertThat(
            new Datarized(less).take(Boolean.class),
            Matchers.equalTo(true)
        );
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0, new Data.ToPhi(42L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Datarized(less).take(Boolean.class),
            Matchers.equalTo(false)
        );
    }

    @Test
    public void rewritesItself() throws Exception {
        final Phi mem = new org.eolang.EOmemory();
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0,
                new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        new Datarized(
            new PhWith(
                new PhCopy(new PhMethod(mem, "write")),
                0,
                new PhWith(
                    new PhMethod(mem, "add"),
                    0,
                    new Data.ToPhi(42L)
                )
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Datarized(mem).take(Long.class),
            Matchers.equalTo(43L)
        );
    }
}
