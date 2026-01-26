/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Win32; // NOPMD

import EOorg.EOeolang.EOsm.Syscall;
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
