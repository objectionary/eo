/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Win32; // NOPMD

import com.sun.jna.IntegerType;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

/**
 * Based on basetsd.h (various types).
 * @since 0.40
 * @checkstyle AbbreviationAsWordInNameCheck (5 lines)
 */
public interface BaseTSD {
    /**
     * Unsigned long ptr.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (5 lines)
     * @checkstyle TypeNameCheck (5 lines)
     */
    final class ULONG_PTR extends IntegerType {
        /**
         * Ctor.
         * @param value Long value
         */
        public ULONG_PTR(final long value) {
            super(Native.POINTER_SIZE, value, true);
        }

        /**
         * Convert to pointer.
         * @return Pointer
         */
        public Pointer toPointer() {
            return Pointer.createConstant(this.longValue());
        }
    }
}
