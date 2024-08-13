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
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

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
