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

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/**
 * Ported from Winbase.h (kernel32.dll/kernel services).
 * Microsoft Windows SDK 6.0A.
 * @since 0.40.0
 * @checkstyle InterfaceIsTypeCheck (500 lines)
 */
@SuppressWarnings({"PMD.ConstantsInInterface", "PMD.LongVariable"})
public interface WinBase extends WinDef, BaseTSD {
    /**
     * Constant value representing an invalid {@link WinNT.HANDLE}.
     * @checkstyle AvoidInlineConditionalsCheck (5 lines)
     */
    WinNT.HANDLE INVALID_HANDLE_VALUE = new WinNT.HANDLE(
        Pointer.createConstant(Native.POINTER_SIZE == 8 ? -1 : 0xFFFFFFFFL)
    );

    /**
     * The SECURITY_ATTRIBUTES structure contains the security descriptor for an
     * object and specifies whether the handle retrieved by specifying this
     * structure is inheritable.
     * @since 0.40
     * @checkstyle AbbreviationAsWordInNameCheck (50 lines)
     * @checkstyle TypeNameCheck (10 lines)
     */
    @Structure.FieldOrder({"dwLength", "lpSecurityDescriptor", "bInheritHandle"})
    final class SECURITY_ATTRIBUTES extends Structure {
        /**
         * The size of the structure, in bytes.
         * @checkstyle VisibilityModifierCheck (6 lines)
         * @checkstyle MemberNameCheck (5 lines)
         */
        public DWORD dwLength;

        /**
         * A pointer to a SECURITY_DESCRIPTOR structure that controls access to the object.
         * @checkstyle VisibilityModifierCheck (6 lines)
         * @checkstyle MemberNameCheck (5 lines)
         */
        public Pointer lpSecurityDescriptor;

        /**
         * A Boolean value that specifies whether the returned handle is inherited when
         * a new process is created.
         * @checkstyle VisibilityModifierCheck (6 lines)
         * @checkstyle MemberNameCheck (5 lines)
         */
        public boolean bInheritHandle;

        /**
         * Ctor.
         */
        public SECURITY_ATTRIBUTES() {
            this.dwLength = new DWORD(size());
        }
    }

    /**
     * The Overlapped structure contains information used in
     * asynchronous (or overlapped) input and output (I/O).
     * @since 0.40.0
     * @checkstyle VisibilityModifierCheck (40 lines)
     * @checkstyle MemberNameCheck (40 lines)
     */
    @Structure.FieldOrder({"Internal", "InternalHigh", "Offset", "OffsetHigh", "hEvent"})
    @SuppressWarnings("PMD.FieldNamingConventions")
    final class OVERLAPPED extends Structure {
        /**
         * Internal.
         */
        public ULONG_PTR Internal;

        /**
         * InternalHigh.
         */
        public ULONG_PTR InternalHigh;

        /**
         * Offset.
         */
        public int Offset;

        /**
         * OffsetHigh.
         */
        public int OffsetHigh;

        /**
         * Event.
         */
        public WinNT.HANDLE hEvent;
    }
}
