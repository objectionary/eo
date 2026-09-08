/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The WSAData structure contains information about the implementation of Windows sockets.
 *
 * @since 0.40.0
 * @checkstyle VisibilityModifierCheck (50 lines)
 * @checkstyle AbbreviationAsWordInNameCheck (100 lines)
 * @checkstyle MemberNameCheck (100 lines)
 */
public final class WSAData extends Structure {

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
    public byte[] description;

    /**
     * Status and configuration description.
     */
    public byte[] systemStatus;

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

    /**
     * Ctor.
     */
    public WSAData() {
        this.description = new byte[257];
        this.systemStatus = new byte[129];
    }

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
