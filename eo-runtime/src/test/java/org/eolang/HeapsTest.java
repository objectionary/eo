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
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Heaps}.
 *
 * @since 0.19
 */
public final class HeapsTest {
    /**
     * Heaps.
     */
    private static final Heaps HEAPS = Heaps.INSTANCE.get();

    @Test
    void allocatesMemory() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 10);
        Assertions.assertDoesNotThrow(
            () -> HeapsTest.HEAPS.read(idx)
        );
    }

    @Test
    void failsOnDoubleAllocation() {
        final Phi phi = new PhFake();
        HeapsTest.HEAPS.malloc(phi, 10);
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.malloc(phi, 10)
        );
    }

    @Test
    void allocatesAndReadsEmptyBytes() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        MatcherAssert.assertThat(
            HeapsTest.HEAPS.read(idx),
            Matchers.equalTo(new byte[] {0, 0, 0, 0, 0})
        );
    }

    @Test
    void writesAndReads() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        final byte[] bytes = new byte[] {1, 2, 3, 4, 5};
        HeapsTest.HEAPS.write(idx, bytes);
        MatcherAssert.assertThat(
            HeapsTest.HEAPS.read(idx),
            Matchers.equalTo(bytes)
        );
    }

    @Test
    void failsOnWriteToEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.write(new PhFake().hashCode(), new byte[] {0x01})
        );
    }

    @Test
    void failsOnReadFromEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.read(new PhFake().hashCode())
        );
    }

    @Test
    void failsOnWriteMoreThanAllocated() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 2);
        final byte[] bytes = new byte[] {1, 2, 3, 4, 5};
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.write(idx, bytes)
        );
    }

    @Test
    void concatsOnWriteLessThanAllocated() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        HeapsTest.HEAPS.write(idx, new byte[] {1, 1, 3, 4, 5});
        HeapsTest.HEAPS.write(idx, new byte[] {2, 2});
        MatcherAssert.assertThat(
            HeapsTest.HEAPS.read(idx),
            Matchers.equalTo(new byte[] {2, 2, 3, 4, 5})
        );
    }

    @Test
    void freesSuccessfully() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        HeapsTest.HEAPS.free(idx);
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.read(idx)
        );
    }

    @Test
    void failsOnClearingEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.free(new PhFake().hashCode())
        );
    }
}
