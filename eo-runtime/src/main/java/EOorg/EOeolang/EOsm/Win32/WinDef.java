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

/**
 * Ported from Windef.h (various macros and types). Microsoft Windows SDK 6.0A.
 * @since 0.40
 */
public interface WinDef {
    /**
     * The 16-bit unsigned integer.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (30 lines)
     */
    final class WORD extends IntegerType implements Comparable<WORD> {

        /**
         * The Constant SIZE.
         */
        private static final int SIZE = 2;

        /**
         * Instantiates a new word.
         * @param value The value
         */
        public WORD(final long value) {
            super(WORD.SIZE, value, true);
        }

        @Override
        public int compareTo(final WORD other) {
            return compare(this, other);
        }

        @Override
        public boolean equals(final Object other) {
            final boolean result;
            if (other != null && this.getClass().equals(other.getClass())) {
                result = this.longValue() == WORD.class.cast(other).longValue();
            } else {
                result = false;
            }
            return result;
        }

        @Override
        public int hashCode() {
            return Long.hashCode(this.longValue());
        }
    }

    /**
     * The 32-bit unsigned integer.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (30 lines)
     */
    final class DWORD extends IntegerType implements Comparable<DWORD> {
        /**
         * Constant size.
         */
        private static final int SIZE = 4;

        /**
         * Ctor.
         * @param value The value
         */
        public DWORD(final long value) {
            super(DWORD.SIZE, value, true);
        }

        /**
         * Low WORD.
         * @return Low WORD.
         * @checkstyle NonStaticMethodCheck (5 lines)
         */
        public WORD getLow() {
            return new WORD(longValue() & 0xFFFF);
        }

        /**
         * High WORD.
         * @return High WORD.
         * @checkstyle NonStaticMethodCheck (5 lines)
         */
        public WORD getHigh() {
            return new WORD((longValue() >> 16) & 0xFFFF);
        }

        @Override
        public int compareTo(final DWORD other) {
            return compare(this, other);
        }

        @Override
        public boolean equals(final Object other) {
            final boolean result;
            if (other != null && this.getClass().equals(other.getClass())) {
                result = this.longValue() == DWORD.class.cast(other).longValue();
            } else {
                result = false;
            }
            return result;
        }

        @Override
        public int hashCode() {
            return Long.hashCode(this.longValue());
        }
    }
}
