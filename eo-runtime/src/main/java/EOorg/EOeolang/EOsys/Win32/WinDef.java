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
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

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
    }
}
