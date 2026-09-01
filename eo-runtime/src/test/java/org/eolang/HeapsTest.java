/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;
import java.util.HashSet;
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
                10, idx -> Heaps.INSTANCE.read(idx, 0, 10)
            ),
            "Heaps should successfully read from allocated memory, but it didn't"
        );
    }

    @Test
    void handsOutDistinctIdentifiersToNestedBlocks() {
        MatcherAssert.assertThat(
            "Heaps must give distinct identifiers to two blocks alive at the same time",
            Heaps.INSTANCE.malloc(
                10,
                outer -> Heaps.INSTANCE.malloc(
                    10,
                    inner -> new HashSet<>(Arrays.asList(outer, inner)).size()
                )
            ),
            Matchers.equalTo(2)
        );
    }

    @Test
    void keepsNestedBlocksApartOnWrite() {
        MatcherAssert.assertThat(
            "A write to the inner block must not corrupt the outer one",
            Heaps.INSTANCE.malloc(
                10,
                outer -> Heaps.INSTANCE.malloc(
                    10,
                    inner -> {
                        Heaps.INSTANCE.write(outer, 0, new byte[] {1});
                        Heaps.INSTANCE.write(inner, 0, new byte[] {2});
                        return Heaps.INSTANCE.read(outer, 0, 1);
                    }
                )
            ),
            Matchers.equalTo(new byte[] {1})
        );
    }

    @Test
    void failsCleanlyOnMallocWithNegativeSize() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(-1, idx -> idx),
            "Heaps must reject a negative malloc size with a clean ExFailure, not a raw JVM exception"
        );
    }

    @Test
    void allocatesAndReadsEmptyBytes() {
        MatcherAssert.assertThat(
            "Heaps should return empty bytes after memory allocation, but it didn't",
            Heaps.INSTANCE.malloc(
                5, idx -> Heaps.INSTANCE.read(idx, 0, 5)
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
                5,
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
            () -> Heaps.INSTANCE.write(Integer.MAX_VALUE, 0, new byte[] {0x01}),
            "Heaps should throw an exception on writing to an unallocated block, but it didn't"
        );
    }

    @Test
    void failsOnWriteIfOffsetPlusLengthOverflows() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                10,
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
                10,
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
                3,
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
            () -> Heaps.INSTANCE.read(Integer.MAX_VALUE, 0, 1),
            "Heaps should throw an exception on reading from an unallocated block, but it didn't"
        );
    }

    @Test
    void failsOnReadIfOutOfBounds() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                2, idx -> Heaps.INSTANCE.read(idx, 1, 3)
            ),
            "Heaps should throw an exception on out-of-bounds read, but it didn't"
        );
    }

    @Test
    void failsOnReadIfOffsetPlusLengthOverflows() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                10,
                idx -> Heaps.INSTANCE.read(idx, Integer.MAX_VALUE - 1, Integer.MAX_VALUE - 1)
            ),
            "Heaps must fail on out-of-bounds read when offset + length overflows int"
        );
    }

    @Test
    void failsCleanlyOnNegativeReadArguments() {
        Heaps.INSTANCE.malloc(
            10,
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
    void keepsEveryByteWrittenOneAtATime() {
        MatcherAssert.assertThat(
            "a block filled one byte at a time must hold every one of them",
            Heaps.INSTANCE.malloc(
                4,
                idx -> {
                    for (int offset = 0; offset < 4; offset += 1) {
                        Heaps.INSTANCE.write(idx, offset, new byte[] {(byte) (offset + 1)});
                    }
                    return Heaps.INSTANCE.read(idx, 0, 4);
                }
            ),
            Matchers.equalTo(new byte[] {1, 2, 3, 4})
        );
    }

    @Test
    void blamesTheOffsetWhenItIsNegative() {
        MatcherAssert.assertThat(
            "a negative read offset must be named as the reason, not the allocated size",
            Heaps.INSTANCE.malloc(
                8,
                idx -> Assertions.assertThrows(
                    ExFailure.class,
                    () -> Heaps.INSTANCE.read(idx, -1, 4),
                    "a negative read offset must be refused"
                ).getMessage()
            ),
            Matchers.containsString("negative offset '-1'")
        );
    }

    @Test
    void blamesTheLengthWhenItIsNegative() {
        MatcherAssert.assertThat(
            "a negative read length must be named as the reason, not the allocated size",
            Heaps.INSTANCE.malloc(
                8,
                idx -> Assertions.assertThrows(
                    ExFailure.class,
                    () -> Heaps.INSTANCE.read(idx, 2, -3),
                    "a negative read length must be refused"
                ).getMessage()
            ),
            Matchers.containsString("negative number of bytes '-3'")
        );
    }

    @Test
    void readsByOffsetAndLength() {
        MatcherAssert.assertThat(
            "Heaps should successfully read correct slice when reading with offset and length, but it didn't",
            Heaps.INSTANCE.malloc(
                5,
                idx -> {
                    Heaps.INSTANCE.write(idx, 0, new byte[] {1, 2, 3, 4, 5});
                    return Heaps.INSTANCE.read(idx, 1, 3);
                }
            ),
            Matchers.equalTo(new byte[] {2, 3, 4})
        );
    }

    @Test
    void freesBlockWhenScopeThrows() {
        final int[] captured = new int[1];
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                10,
                idx -> {
                    captured[0] = idx;
                    return Heaps.INSTANCE.read(idx, 0, 20);
                }
            ),
            "Heaps should propagate the failure raised inside the scope, but it didn't"
        );
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.size(captured[0]),
            "Heaps must free the block when the scope throws, but it left it allocated"
        );
    }

    @Test
    void rejectsWritePastAllocatedBlock() {
        final byte[] original = {7, 8, 9};
        MatcherAssert.assertThat(
            "Heaps must leave the allocated block unchanged after rejecting an out-of-bounds write",
            Heaps.INSTANCE.malloc(
                original.length,
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
                0,
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
                5,
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
        final int idx = Heaps.INSTANCE.malloc(5, ident -> ident);
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
            () -> Heaps.INSTANCE.size(Integer.MAX_VALUE),
            "Heaps should throw an exception on trying to get size of an empty block, but it didn't"
        );
    }

    @Test
    void returnsValidSize() {
        MatcherAssert.assertThat(
            "Heaps should return valid size of allocated block, but it didn't",
            Heaps.INSTANCE.malloc(5, Heaps.INSTANCE::size),
            Matchers.equalTo(5)
        );
    }

    @Test
    void throwsOnChangingSizeToNegative() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Heaps.INSTANCE.malloc(
                5,
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
            () -> Heaps.INSTANCE.resize(Integer.MAX_VALUE, 10),
            "Heaps should throw an exception on changing size of empty block, but it didn't"
        );
    }

    @Test
    void increasesSizeSuccessfully() {
        MatcherAssert.assertThat(
            "Heaps should successfully increase size of allocated block, but it didn't",
            Heaps.INSTANCE.malloc(
                5,
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
                5,
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
                5,
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
}
