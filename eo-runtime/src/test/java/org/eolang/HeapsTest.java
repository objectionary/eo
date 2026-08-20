/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Supplier;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Heaps}.
 * @since 0.19
 */
final class HeapsTest {

    @Test
    void allocatesMemory() {
        Assertions.assertDoesNotThrow(
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 10, idx -> Heaps.INSTANCE.read(idx, 0, 10)
            ),
            "Heaps should successfully read from allocated memory, but it didn't"
        );
    }

    @Test
    void failsOnDoubleAllocation() {
        final Phi phi = new HeapsTest.PhFake();
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                phi, 10, idx -> Heaps.INSTANCE.malloc(phi, 10, Heaps.INSTANCE::size)
            ),
            "Heaps should throw an exception on attempting to allocate already allocated memory, but it didn't"
        );
    }

    @Test
    void failsCleanlyOnMallocWithNegativeSize() {
        final Phi phi = new HeapsTest.PhFake();
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(phi, -1, idx -> idx),
            "Heaps must reject a negative malloc size with a clean ExFailure, not a raw JVM exception"
        );
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.size(phi.hashCode()),
            "Heaps must not leave a block allocated after a rejected negative malloc, but it did"
        );
    }

    @Test
    void allocatesAndReadsEmptyBytes() {
        MatcherAssert.assertThat(
            "Heaps should return empty bytes after memory allocation, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5, idx -> Heaps.INSTANCE.read(idx, 0, 5)
            ),
            Matchers.equalTo(new byte[] {0, 0, 0, 0, 0})
        );
    }

    @Test
    void writesAndReads() {
        final byte[] bytes = {1, 2, 3, 4, 5};
        MatcherAssert.assertThat(
            "Heaps should successfully read exactly same bytes that were written, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, bytes);
                    return Heaps.INSTANCE.read(idx, 0, bytes.length);
                }
            ),
            Matchers.equalTo(bytes)
        );
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
    void failsOnWriteIfOffsetPlusLengthOverflows() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 10,
                idx -> {
                    Heaps.INSTANCE.write(idx, Integer.MAX_VALUE, new byte[] {0x01});
                    return idx;
                }
            ),
            "Heaps should throw an exception on write when offset + data.length overflows int, but it didn't"
        );
    }

    @Test
    void failsCleanlyOnWriteWithNegativeOffset() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 10,
                idx -> {
                    Heaps.INSTANCE.write(idx, -1, new byte[] {0x01});
                    return idx;
                }
            ),
            "Heaps must reject a negative write offset with a clean ExFailure, not a raw JVM exception"
        );
    }

    @Test
    void keepsBlockIntactAfterWriteWithNegativeOffset() {
        MatcherAssert.assertThat(
            "Heaps must leave the block untouched after a rejected negative-offset write, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 3,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {7, 8, 9});
                    Assertions.assertThrows(
                        ExFailure.class,
                        () -> Heaps.INSTANCE.write(idx, -2, new byte[] {1, 2}),
                        "Heaps must reject a negative write offset before touching the block, but it didn't"
                    );
                    return Heaps.INSTANCE.read(idx, 0, 3);
                }
            ),
            Matchers.equalTo(new byte[] {7, 8, 9})
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
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 2, idx -> Heaps.INSTANCE.read(idx, 1, 3)
            ),
            "Heaps should throw an exception on out-of-bounds read, but it didn't"
        );
    }

    @Test
    void failsOnReadIfOffsetPlusLengthOverflows() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 10,
                idx -> Heaps.INSTANCE.read(idx, Integer.MAX_VALUE - 1, Integer.MAX_VALUE - 1)
            ),
            "Heaps must fail on out-of-bounds read when offset + length overflows int"
        );
    }

    @Test
    void failsCleanlyOnNegativeReadArguments() {
        Heaps.INSTANCE.malloc(
            new HeapsTest.PhFake(), 10,
            idx -> {
                Assertions.assertThrows(
                    ExFailure.class,
                    () -> Heaps.INSTANCE.read(idx, -5, 3),
                    "Heaps must reject a negative offset with a clean ExFailure, not a raw JVM exception"
                );
                return Assertions.assertThrows(
                    ExFailure.class,
                    () -> Heaps.INSTANCE.read(idx, 2, -3),
                    "Heaps must reject a negative length with a clean ExFailure, not a raw JVM exception"
                );
            }
        );
    }

    @Test
    void readsByOffsetAndLength() {
        MatcherAssert.assertThat(
            "Heaps should successfully read correct slice when reading with offset and length, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
                    return Heaps.INSTANCE.read(idx, 1, 3);
                }
            ),
            Matchers.equalTo(new byte[] {2, 3, 4})
        );
    }

    @Test
    void reusesObjectAfterScopeThrows() {
        final Phi phi = new HeapsTest.PhFake();
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(phi, 10, idx -> Heaps.INSTANCE.read(idx, 0, 20)),
            "Heaps should propagate the failure raised inside the scope, but it didn't"
        );
        MatcherAssert.assertThat(
            "Heaps must free the block when the scope throws, but the same object could not allocate",
            Heaps.INSTANCE.malloc(phi, 5, Heaps.INSTANCE::size),
            Matchers.equalTo(5)
        );
    }

    @Test
    void rejectsWritePastAllocatedBlock() {
        final byte[] original = {7, 8, 9};
        MatcherAssert.assertThat(
            "Heaps must leave the allocated block unchanged after rejecting an out-of-bounds write",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), original.length,
                idx -> HeapsTest.rejectedWrite(idx, original)
            ),
            Matchers.equalTo(original)
        );
    }

    @Test
    void rejectsWritePastZeroSizedBlock() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 0,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1});
                    return idx;
                }
            ),
            "Heaps must not implicitly grow a zero-sized block"
        );
    }

    @Test
    void concatsOnWriteLessThanAllocated() {
        MatcherAssert.assertThat(
            "Heaps should return correct bytes after partial overwrite preserving trailing bytes, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1, 1, 3, 4, 5});
                    Heaps.INSTANCE.write(idx, 2, new byte[] {2, 2});
                    return Heaps.INSTANCE.read(idx, 0, 5);
                }
            ),
            Matchers.equalTo(new byte[] {1, 1, 2, 2, 5})
        );
    }

    @Test
    void freesSuccessfully() {
        final int idx = Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5, ident -> ident);
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.read(idx, 0, 5),
            "Heaps should throw an exception on reading from a freed block, but it didn't"
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
        MatcherAssert.assertThat(
            "Heaps should return valid size of allocated block, but it didn't",
            Heaps.INSTANCE.malloc(new HeapsTest.PhFake(), 5, Heaps.INSTANCE::size),
            Matchers.equalTo(5)
        );
    }

    @Test
    void throwsOnChangingSizeToNegative() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.resize(idx, -1);
                    return idx;
                }
            ),
            "Heaps should throw an exception on trying to changing size to negative, but it didn't"
        );
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
        MatcherAssert.assertThat(
            "Heaps should successfully increase size of allocated block, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
                    Heaps.INSTANCE.resize(idx, 7);
                    return Heaps.INSTANCE.read(idx, 0, 7);
                }
            ),
            Matchers.equalTo(new byte[] {1, 2, 3, 4, 5, 0, 0})
        );
    }

    @Test
    void decreasesSizeSuccessfully() {
        MatcherAssert.assertThat(
            "Heaps should successfully decrease size of allocated block, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
                    Heaps.INSTANCE.resize(idx, 3);
                    return Heaps.INSTANCE.read(idx, 0, 3);
                }
            ),
            Matchers.equalTo(new byte[] {1, 2, 3})
        );
    }

    @Test
    void returnsValidSizeAfterDecreasing() {
        MatcherAssert.assertThat(
            "Heaps should return valid size after decreasing, but it didn't",
            Heaps.INSTANCE.malloc(
                new HeapsTest.PhFake(), 5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
                    Heaps.INSTANCE.resize(idx, 3);
                    return Heaps.INSTANCE.size(idx);
                }
            ),
            Matchers.equalTo(3)
        );
    }

    private static byte[] rejectedWrite(final int identifier, final byte[] original) {
        Heaps.INSTANCE.write(identifier, 0, original);
        MatcherAssert.assertThat(
            "The failure must describe the rejected write and allocated block",
            Assertions.assertThrows(
                ExFailure.class,
                () -> Heaps.INSTANCE.write(identifier, 100, new byte[] {1})
            ).getMessage(),
            Matchers.equalTo(
                String.format(
                    "Can't write '1' bytes with offset '100' to the block with identifier '%d', because only '3' were allocated",
                    identifier
                )
            )
        );
        MatcherAssert.assertThat(
            "A rejected write must not change the allocated size",
            Heaps.INSTANCE.size(identifier),
            Matchers.equalTo(original.length)
        );
        return Heaps.INSTANCE.read(identifier, 0, original.length);
    }

    /**
     * Fake object, mostly for unit tests.
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
         * @checkstyle ConstructorsCodeFreeCheck (10 lines)
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        PhFake(final Supplier<Phi> sup) {
            this.add("args", new AtVoid("args"));
            this.add("φ", new AtComposite(this, rho -> sup.get()));
        }
    }
}
