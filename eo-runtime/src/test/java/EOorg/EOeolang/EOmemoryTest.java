/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOmemory}.
 *
 * @since 0.1
 */
public final class EOmemoryTest {

    /**
     * Method name.
     */
    private static final String WRITE = "write";

    @Test
    public void writeAfterCopy() {
        final Phi first = new EOmemory(Phi.Φ);
        final Phi second = first.copy();
        second.attr(0).put(new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(second).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void readsAndWrites() {
        final Phi mem = new EOmemory(Phi.Φ);
        final Phi text = new Data.ToPhi("Hello, world!");
        final Phi write = mem.attr(EOmemoryTest.WRITE).get();
        write.attr(0).put(text);
        new Dataized(write).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(mem).take(String.class),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    public void comparesForEquality() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhCopy(new PhMethod(mem, "eq")),
                    0,
                    new Data.ToPhi(1L)
                )
            ).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    public void writesAndRewrites() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(mem).take(Long.class),
            Matchers.equalTo(1L)
        );
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(5L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(mem).take(Long.class),
            Matchers.equalTo(5L)
        );
    }

    @Test
    public void makeCorrectCopy() {
        final Phi mem = new EOmemory(Phi.Φ);
        final Phi text = new Data.ToPhi(1L);
        final Phi write = mem.attr(EOmemoryTest.WRITE).get();
        write.attr(0).put(text);
        new Dataized(write).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(new PhCopy(mem)).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void makesTrueCopy() {
        final Phi first = new EOmemory(Phi.Φ);
        first.attr(0).put(new Data.ToPhi(1L));
        final Phi second = first.copy();
        new Dataized(
            new PhWith(
                second.attr("write").get(),
                "x", new Data.ToPhi(2L)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(first).take(Long.class),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            new Dataized(second).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    @Test
    public void comparesOnFly() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        final Phi less = new PhWith(
            new PhCopy(new PhMethod(mem, "lt")),
            0, new Data.ToPhi(10L)
        );
        MatcherAssert.assertThat(
            new Dataized(less).take(Boolean.class),
            Matchers.equalTo(true)
        );
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(42L)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(less).take(Boolean.class),
            Matchers.equalTo(false)
        );
    }

    @Test
    public void rewritesItself() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0,
                new Data.ToPhi(1L)
            )
        ).take(Boolean.class);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0,
                new PhWith(
                    new PhCopy(new PhMethod(mem, "plus")),
                    0, new Data.ToPhi(42L)
                )
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(mem).take(Long.class),
            Matchers.equalTo(43L)
        );
    }

}
