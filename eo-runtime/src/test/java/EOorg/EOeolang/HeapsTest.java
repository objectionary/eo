/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtCompositeTest;
import org.eolang.ExFailure;
import org.eolang.PhFake;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Heaps}.
 *
 * @since 0.19
 */
@SuppressWarnings("PMD.TooManyMethods")
final class HeapsTest {
    /**
     * Heaps.
     */
    private static final Heaps HEAPS = Heaps.INSTANCE;

    @Test
    void allocatesMemory() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 10);
        Assertions.assertDoesNotThrow(
            () -> HeapsTest.HEAPS.read(idx, 0, 10),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void failsOnDoubleAllocation() {
        final Phi phi = new PhFake();
        final int idx = HeapsTest.HEAPS.malloc(phi, 10);
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.malloc(phi, 10),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void allocatesAndReadsEmptyBytes() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            HeapsTest.HEAPS.read(idx, 0, 5),
            Matchers.equalTo(new byte[] {0, 0, 0, 0, 0})
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void writesAndReads() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        final byte[] bytes = {1, 2, 3, 4, 5};
        HeapsTest.HEAPS.write(idx, 0, bytes);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            HeapsTest.HEAPS.read(idx, 0, bytes.length),
            Matchers.equalTo(bytes)
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void failsOnWriteToEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.write(new PhFake().hashCode(), 0, new byte[] {0x01}),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void failsOnReadFromEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.read(new PhFake().hashCode(), 0, 1),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void failsOnReadIfOutOfBounds() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 2);
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.read(idx, 1, 3),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void readsByOffsetAndLength() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        HeapsTest.HEAPS.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            HeapsTest.HEAPS.read(idx, 1, 3),
            Matchers.equalTo(new byte[] {2, 3, 4})
        );
    }

    @Test
    void failsOnWriteMoreThanAllocated() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 2);
        final byte[] bytes = {1, 2, 3, 4, 5};
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.write(idx, 0, bytes),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void failsToWriteMoreThanAllocatedWithOffset() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 3);
        final byte[] bytes = {1, 2, 3};
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.write(idx, 1, bytes),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void concatsOnWriteLessThanAllocated() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        HeapsTest.HEAPS.write(idx, 0, new byte[] {1, 1, 3, 4, 5});
        HeapsTest.HEAPS.write(idx, 2, new byte[] {2, 2});
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            HeapsTest.HEAPS.read(idx, 0, 5),
            Matchers.equalTo(new byte[] {1, 1, 2, 2, 5})
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void freesSuccessfully() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        HeapsTest.HEAPS.free(idx);
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.read(idx, 0, 5),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void failsOnClearingEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.free(new PhFake().hashCode()),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void throwsOnGettingSizeOfEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.size(new PhFake().hashCode()),
            "Heaps should throw an exception if trying to get size on an empty block, but it didn't"
        );
    }

    @Test
    void returnsValidSize() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        MatcherAssert.assertThat(
            "Heaps should return valid size of allocated block, but it didn't",
            HeapsTest.HEAPS.size(idx),
            Matchers.equalTo(5)
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void throwsOnChangingSizeToNegative() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.resize(idx, -1),
            "Heaps should throw an exception on trying to changing size to negative, but it didn't"
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void throwsOnChangeSizeOfEmtpyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> HeapsTest.HEAPS.resize(new PhFake().hashCode(), 10),
            "Heaps should throw an exception on changing size of empty block, but it didn't"
        );
    }

    @Test
    void increasesSizeSuccessfully() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        final byte[] bytes = {1, 2, 3, 4, 5};
        HeapsTest.HEAPS.write(idx, 0, bytes);
        HeapsTest.HEAPS.resize(idx, 7);
        MatcherAssert.assertThat(
            "Heaps should successfully increase size of allocated block, but it didn't",
            HeapsTest.HEAPS.read(idx, 0, 7),
            Matchers.equalTo(new byte[] {1, 2, 3, 4, 5, 0, 0})
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void decreasesSizeSuccessfully() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        final byte[] bytes = {1, 2, 3, 4, 5};
        HeapsTest.HEAPS.write(idx, 0, bytes);
        HeapsTest.HEAPS.resize(idx, 3);
        MatcherAssert.assertThat(
            "Heaps should successfully decrease size of allocated block, but it didn't",
            HeapsTest.HEAPS.read(idx, 0, 3),
            Matchers.equalTo(new byte[] {1, 2, 3})
        );
        HeapsTest.HEAPS.free(idx);
    }

    @Test
    void returnsValidSizeAfterDecreasing() {
        final int idx = HeapsTest.HEAPS.malloc(new PhFake(), 5);
        final byte[] bytes = {1, 2, 3, 4, 5};
        HeapsTest.HEAPS.write(idx, 0, bytes);
        HeapsTest.HEAPS.resize(idx, 3);
        MatcherAssert.assertThat(
            "Heaps should return valid size after decreasing, but it didn't",
            HeapsTest.HEAPS.size(idx),
            Matchers.equalTo(3)
        );
        HeapsTest.HEAPS.free(idx);
    }

}
