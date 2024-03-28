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
        final Phi alloc = EOmemoryTest.allocated(new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(alloc.take("as-int")).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void hasTheSameDataAfterCopy() {
        final Phi alloc = EOmemoryTest.allocated(new Data.ToPhi(2L));
        MatcherAssert.assertThat(
            new Dataized(alloc).take(),
            Matchers.equalTo(new Dataized(alloc.copy()).take())
        );
    }

    @Test
    void writesDataToNewMemorySegment() {
        final Phi first = EOmemoryTest.allocated(new Data.ToPhi(1L));
        final Phi second = EOmemoryTest.allocated(new Data.ToPhi(2L));
        MatcherAssert.assertThat(
            new Dataized(first).take(),
            Matchers.not(Matchers.equalTo(new Dataized(second).take()))
        );
    }

    @Test
    void rewritesToTheSameSegment() {
        final Phi alloc = EOmemoryTest.allocated(new Data.ToPhi(1L));
        new Dataized(new PhWith(alloc.take("write").copy(), 0, new Data.ToPhi(10L))).take();
        MatcherAssert.assertThat(
            new Dataized(alloc).take(Long.class),
            Matchers.equalTo(10L)
        );
    }

    @Test
    void takesAsIntAndUpdates() {
        final Phi alloc = EOmemoryTest.allocated(new Data.ToPhi(1L));
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    alloc.take("as-int").take("plus").copy(),
                    0, new Data.ToPhi(1L)
                )
            ).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    @Test
    void getsWrittenValueRightAfterWriting() {
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(1L));
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
    void comparesForEquality() {
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(2L));
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
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(10L));
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
    void comparesOnFly() {
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(10L));
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
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(1L));
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
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(true));
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
        final Phi mem = EOmemoryTest.allocated(new Data.ToPhi(2L));
        final Phi write = mem.take(EOmemoryTest.WRITE);
        new Dataized(new PhWith(write.copy(), 0, new Data.ToPhi(true))).take();
        Assertions.assertDoesNotThrow(
            () -> new Dataized(
                new PhWith(write.copy(), 0, new Data.ToPhi(1L))
            ).take()
        );
    }

    /**
     * Allocated data.
     * @param obj Object to put
     * @return Alloc object
     */
    private static Phi allocated(final Phi obj) {
        final Phi memory = Phi.Φ.take("org.eolang.memory");
        final Phi alloc = memory.take("alloc").copy();
        alloc.put(0, obj);
        return alloc;
    }
}
