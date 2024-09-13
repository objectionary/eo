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

import EOorg.EOeolang.EOsys.Syscall;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * WSAStartup WS2_32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsastartup">here for details</a>
 * @since 0.40.0
 * @checkstyle AbbreviationAsWordInNameCheck (100 lines)
 */
public final class WSAStartupFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public WSAStartupFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Winsock.INSTANCE.WSAStartup(
                    new Dataized(params[0]).take(Short.class),
                    new WSAStartupFuncCall.WSAData()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }

    /**
     * The WSAData structure contains information about the implementation of Windows sockets.
     * @since 0.40.0
     * @checkstyle VisibilityModifierCheck (50 lines)
     * @checkstyle MemberNameCheck (100 lines)
     */
    public static final class WSAData extends Structure {
        /**
         * Version.
         */
        public short version;

        /**
         * Highest version.
         */
        public short highVersion;

        /**
         * Socket function implementation description.
         */
        public byte[] description = new byte[257];

        /**
         * Status and configuration description.
         */
        public byte[] systemStatus = new byte[129];

        /**
         * Max amount of sockets that can be opened.
         */
        public short maxSockets;

        /**
         * Max size of datagram message. Ignored for sockets Windows 2 and more.
         */
        public short maxUdpDg;

        /**
         * Vendor info. Ignored for sockets Windows 2 and more.
         */
        public Pointer vendorInfo;

        @Override
        public List<String> getFieldOrder() {
            return Arrays.asList(
                "version",
                "highVersion",
                "description",
                "systemStatus",
                "maxSockets",
                "maxUdpDg",
                "vendorInfo"
            );
        }
    }
}
