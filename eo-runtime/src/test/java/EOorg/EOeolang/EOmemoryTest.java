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

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOmemory}.
 *
 * @since 0.1
 * @todo #2931:30min Decide what to do with test {@link EOmemoryTest#comparesOnFly}.
 *  The test was disabled because it does not pass after new rho logic was introduced.
 *  We need either to delete the test, or resolve it somehow
 */
public final class EOmemoryTest {

    /**
     * Method name.
     */
    private static final String WRITE = "write";

    @Test
    void behavesAsBytes() {
        final Phi mem = new EOmemory(Phi.Φ);
        mem.put(0, new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(mem).take(),
            Matchers.instanceOf(byte[].class)
        );
    }

    @Test
    void rewritesAfterInit() {
        final Phi mem = new EOmemory(Phi.Φ);
        mem.put(0, new Data.ToPhi(0L));
        final Phi write = mem.take("write");
        final Phi first = write.copy();
        first.put(0, new Data.ToPhi(42L));
        new Dataized(first).take();
        final Phi minus = mem.take("as-int").take("minus").copy();
        minus.put(0, new Data.ToPhi(2L));
        final Phi second = write.copy();
        second.put(0, minus);
        MatcherAssert.assertThat(
            new Dataized(second).take(Long.class),
            Matchers.equalTo(40L)
        );
    }

    @Test
    void takesAsIntAndUpdates() {
        final Phi mem = new EOmemory(Phi.Φ);
        mem.put(0, new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    mem.take("as-int").take("plus").copy(),
                    0, new Data.ToPhi(1L)
                )
            ).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    @Test
    void getsWrittenValueRightAfterWriting() {
        final Phi mem = new EOmemory(Phi.Φ);
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    mem.take(EOmemoryTest.WRITE).copy(),
                    0, new Data.ToPhi(10L)
                ).take("as-int")
            ).take(Long.class),
            Matchers.equalTo(10L)
        );
    }

    @Test
    void writesAfterCopy() {
        final Phi mem = new EOmemory(Phi.Φ);
        mem.put(0,new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(mem.take("as-int")).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void readsAndWrites() {
        final Phi mem = new EOmemory(Phi.Φ);
        final Phi write = mem.take(EOmemoryTest.WRITE);
        write.put(0,new Data.ToPhi("Hello, world!"));
        new Dataized(write).take();
        MatcherAssert.assertThat(
            new Dataized(mem.take("as-string")).take(String.class),
            Matchers.startsWith("Hello, ")
        );
    }

    @Test
    void comparesForEquality() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhCopy(new PhMethod(mem, "eq")),
                    0, new Data.ToPhi(1L)
                )
            ).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    void writesAndRewrites() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(mem.take("as-int")).take(Long.class),
            Matchers.equalTo(1L)
        );
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(5L)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(mem.take("as-int")).take(Long.class),
            Matchers.equalTo(5L)
        );
    }

    @Test
    void makesCorrectCopy() {
        final Phi mem = new EOmemory(Phi.Φ);
        final Phi write = mem.take(EOmemoryTest.WRITE);
        write.put(0,new Data.ToPhi(1L));
        new Dataized(write).take();
        MatcherAssert.assertThat(
            new Dataized(new PhCopy(mem.take("as-int"))).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void makesTrueCopy() {
        final Phi first = new EOmemory(Phi.Φ);
        first.put(0,new Data.ToPhi(1L));
        final Phi second = first.copy();
        new Dataized(
            new PhWith(
                second.take("write"),
                "x", new Data.ToPhi(2L)
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(first.take("as-int")).take(Long.class),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            new Dataized(second.take("as-int")).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    @Test
    void comparesOnFly() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take();
        final Phi less = new PhWith(
            mem.take("as-int").take("lt").copy(),
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
        ).take();
        MatcherAssert.assertThat(
            new Dataized(less).take(Boolean.class),
            Matchers.equalTo(false)
        );
    }

    @Test
    void rewritesItself() {
        final Phi mem = new EOmemory(Phi.Φ);
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0, new Data.ToPhi(1L)
            )
        ).take();
        new Dataized(
            new PhWith(
                new PhCopy(new PhMethod(mem, EOmemoryTest.WRITE)),
                0,
                new PhWith(
                    mem.take("as-int").take("plus").copy(),
                    0, new Data.ToPhi(42L)
                )
            )
        ).take();
        MatcherAssert.assertThat(
            new Dataized(mem.take("as-int")).take(Long.class),
            Matchers.equalTo(43L)
        );
    }

    @Test
    void doesNotWriteMoreThanAllocated() {
        final Phi mem = new EOmemory(Phi.Φ);
        mem.put(0,new Data.ToPhi(true));
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(
                new PhWith(
                    mem.take(EOmemoryTest.WRITE),
                    0, new Data.ToPhi(8L)
                )
            ).take()
        );
    }

    @Test
    void writesLessAndRewritesTheSame() {
        final Phi mem = new EOmemory(Phi.Φ);
        mem.put(0,new Data.ToPhi(2L));
        final Phi write = mem.take(EOmemoryTest.WRITE);
        new Dataized(new PhWith(write.copy(), 0, new Data.ToPhi(true))).take();
        Assertions.assertDoesNotThrow(
            () -> new Dataized(
                new PhWith(write.copy(), 0, new Data.ToPhi(1L))
            ).take()
        );
    }
}
