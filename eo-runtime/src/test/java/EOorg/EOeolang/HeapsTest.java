/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.function.Supplier;
import org.eolang.AtComposite;
import org.eolang.AtVoid;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
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

    @Test
    void allocatesMemory() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 10);
        Assertions.assertDoesNotThrow(
            () -> Heaps.INSTANCE.read(idx, 0, 10),
            "Heaps should successfully read from allocated memory, but it didn't"
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void failsOnDoubleAllocation() {
        final Phi phi = new HeapsTest.PhFake();
        final int idx = Heaps.INSTANCE.malloc(phi, 10);
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(phi, 10),
            "Heaps should throw an exception on attempting to allocate already allocated memory, but it didn't"
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void allocatesAndReadsEmptyBytes() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        MatcherAssert.assertThat(
            "Heaps should return empty bytes after memory allocation, but it didn't",
            Heaps.INSTANCE.read(idx, 0, 5),
            Matchers.equalTo(new byte[] {0, 0, 0, 0, 0})
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void writesAndReads() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        final byte[] bytes = {1, 2, 3, 4, 5};
        Heaps.INSTANCE.write(idx, 0, bytes);
        MatcherAssert.assertThat(
            "Heaps should successfully read exactly same bytes that were written, but it didn't",
            Heaps.INSTANCE.read(idx, 0, bytes.length),
            Matchers.equalTo(bytes)
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void failsOnWriteToEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.write(new HeapsTest.PhFake().hashCode(), 0, new byte[] {0x01}),
            "Heaps should throw an exception on writing to an unallocated block, but it didn't"
        );
    }

    @Test
    void failsOnReadFromEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.read(new HeapsTest.PhFake().hashCode(), 0, 1),
            "Heaps should throw an exception on reading from an unallocated block, but it didn't"
        );
    }

    @Test
    void failsOnReadIfOutOfBounds() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 2);
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.read(idx, 1, 3),
            "Heaps should throw an exception on out-of-bounds read, but it didn't"
        );
    }

    @Test
    void readsByOffsetAndLength() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
        MatcherAssert.assertThat(
            "Heaps should successfully read correct slice when reading with offset and length, but it didn't",
            Heaps.INSTANCE.read(idx, 1, 3),
            Matchers.equalTo(new byte[] {2, 3, 4})
        );
    }

    @Test
    void failsOnWriteMoreThanAllocated() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 2);
        final byte[] bytes = {1, 2, 3, 4, 5};
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.write(idx, 0, bytes),
            "Heaps should throw an exception on writing more bytes than allocated size, but it didn't"
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void failsToWriteMoreThanAllocatedWithOffset() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 3);
        final byte[] bytes = {1, 2, 3};
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.write(idx, 1, bytes),
            "Heaps should throw an exception on writing past allocated block using offset, but it didn't"
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void concatsOnWriteLessThanAllocated() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Heaps.INSTANCE.write(idx, 0, new byte[] {1, 1, 3, 4, 5});
        Heaps.INSTANCE.write(idx, 2, new byte[] {2, 2});
        MatcherAssert.assertThat(
            "Heaps should return correct bytes after partial overwrite, but it didn't",
            Heaps.INSTANCE.read(idx, 0, 5),
            Matchers.equalTo(new byte[] {1, 1, 2, 2, 5})
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void freesSuccessfully() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Heaps.INSTANCE.free(idx);
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.read(idx, 0, 5),
            "Heaps should throw an exception on reading from a freed block, but it didn't"
        );
    }

    @Test
    void failsOnClearingEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.free(new HeapsTest.PhFake().hashCode()),
            "Heaps should throw an exception on attempting to free a non-existent block, but it didn't"
        );
    }

    @Test
    void throwsOnGettingSizeOfEmptyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.size(new HeapsTest.PhFake().hashCode()),
            "Heaps should throw an exception on trying to get size of an empty block, but it didn't"
        );
    }

    @Test
    void returnsValidSize() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        MatcherAssert.assertThat(
            "Heaps should return valid size of allocated block, but it didn't",
            Heaps.INSTANCE.size(idx),
            Matchers.equalTo(5)
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void throwsOnChangingSizeToNegative() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.resize(idx, -1),
            "Heaps should throw an exception on trying to changing size to negative, but it didn't"
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void throwsOnChangeSizeOfEmtpyBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.resize(new HeapsTest.PhFake().hashCode(), 10),
            "Heaps should throw an exception on changing size of empty block, but it didn't"
        );
    }

    @Test
    void increasesSizeSuccessfully() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
        Heaps.INSTANCE.resize(idx, 7);
        MatcherAssert.assertThat(
            "Heaps should successfully increase size of allocated block, but it didn't",
            Heaps.INSTANCE.read(idx, 0, 7),
            Matchers.equalTo(new byte[] {1, 2, 3, 4, 5, 0, 0})
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void decreasesSizeSuccessfully() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
        Heaps.INSTANCE.resize(idx, 3);
        MatcherAssert.assertThat(
            "Heaps should successfully decrease size of allocated block, but it didn't",
            Heaps.INSTANCE.read(idx, 0, 3),
            Matchers.equalTo(new byte[] {1, 2, 3})
        );
        Heaps.INSTANCE.free(idx);
    }

    @Test
    void returnsValidSizeAfterDecreasing() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5);
        Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
        Heaps.INSTANCE.resize(idx, 3);
        MatcherAssert.assertThat(
            "Heaps should return valid size after decreasing, but it didn't",
            Heaps.INSTANCE.size(idx),
            Matchers.equalTo(3)
        );
        Heaps.INSTANCE.free(idx);
    }

    /**
     * Fake object, mostly for unit tests.
     *
     * @since 0.29
     */
    private static final class PhFake extends PhDefault {
        /**
         * Ctor.
         */
        PhFake() {
            this(() -> Phi.Φ);
        }

        /**
         * Ctor.
         * @param sup The function to return the real object
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PhFake(final Supplier<Phi> sup) {
            this.add("args", new AtVoid("args"));
            this.add("φ", new AtComposite(this, rho -> sup.get()));
        }
    }
}
