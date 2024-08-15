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

import com.sun.jna.FromNativeContext;
import com.sun.jna.Pointer;
import com.sun.jna.PointerType;

/**
 * This module defines the 32-Bit Windows types and constants that are defined
 * by NT, but exposed through the Win32 API. Ported from WinNT.h Microsoft
 * Windows SDK 6.0A.
 * @since 0.40
 * @checkstyle InterfaceIsTypeCheck (500 lines)
 */
@SuppressWarnings("PMD.LongVariable")
public interface WinNT extends WinDef, WinBase, BaseTSD {
    /**
     * This flag specifies the file's attributes. FILE_ATTRIBUTE_NORMAL indicates that the file
     * does not have any special attributes.
     * Value: 0x80 (or 128 in decimal)
     * Behavior:
     * The file is a standard file with no special attributes, such as hidden, system, or read-only.
     */
    int FILE_ATTRIBUTE_NORMAL = 0x00000080;

    /**
     * This flag determines how a file should be handled if it already exists or doesn't
     * exist when you attempt to create or open it.
     * Behavior:
     * If the file already exists, it will be overwritten (truncated to zero length).
     * If the file does not exist, it will be created.
     */
    int CREATE_ALWAYS = 2;

    /**
     * The OPEN_EXISTING flag tells the CreateFile function to open the file only if
     * it already exists. If the file doesn't exist, the function fails, and an
     * error is returned (typically ERROR_FILE_NOT_FOUND).
     */
    int OPEN_EXISTING = 3;

    /**
     * This flag specifies the desired access to the file. GENERIC_WRITE allows for writing
     * data to the file.
     * Value: 0x40000000 (or 1073741824 in decimal)
     * The file can be written to, meaning you can modify its contents.
     * This flag grants the ability to write data, append data, or modify file attributes.
     */
    int GENERIC_WRITE = 0x40000000;

    /**
     * This flag grants read access to the file or resource.
     * When you specify GENERIC_READ, the handle you obtain will allow you to read the
     * contents of the file or resource.
     * Value: 0x80000000
     */
    int GENERIC_READ = 0x80000000;

    /**
     * Handle to an object.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (100 lines)
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    final class HANDLE extends PointerType {
        /**
         * Whether pointer is immutable.
         */
        private final boolean immutable;

        /**
         * Ctor.
         */
        public HANDLE() {
            this(null, false);
        }

        /**
         * Ctor.
         * @param ptr Pointer
         */
        public HANDLE(final Pointer ptr) {
            this(ptr, true);
        }

        /**
         * Ctor.
         * @param ptr Pointer
         * @param immutable Immutable
         */
        private HANDLE(final Pointer ptr, final boolean immutable) {
            super.setPointer(ptr);
            this.immutable = immutable;
        }

        @Override
        public Object fromNative(final Object value, final FromNativeContext context) {
            final Object obj = super.fromNative(value, context);
            final Object result;
            if (INVALID_HANDLE_VALUE.equals(obj)) {
                result = INVALID_HANDLE_VALUE;
            } else {
                result = obj;
            }
            return result;
        }

        @Override
        public void setPointer(final Pointer ptr) {
            if (this.immutable) {
                throw new UnsupportedOperationException("immutable reference");
            }
            super.setPointer(ptr);
        }

        @Override
        public String toString() {
            return String.valueOf(getPointer());
        }
    }
}
