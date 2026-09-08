/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The {@code struct _stat64} of the Microsoft C runtime.
 *
 * @since 0.74.0
 * @checkstyle VisibilityModifierCheck (60 lines)
 */
public final class WinStat extends Structure {

    /**
     * Device id.
     */
    public int dev;

    /**
     * Inode number.
     */
    public short ino;

    /**
     * Mode bits.
     */
    public short mode;

    /**
     * Hard link count.
     */
    public short nlink;

    /**
     * Owner id.
     */
    public short uid;

    /**
     * Group id.
     */
    public short gid;

    /**
     * Padding before the following device id.
     */
    public short padding;

    /**
     * Device id for special files.
     */
    public int rdev;

    /**
     * Size in bytes.
     */
    public long bytes;

    /**
     * Access, modification and change 64-bit timestamps EO does not read.
     */
    public byte[] times;

    /**
     * Ctor.
     */
    public WinStat() {
        super();
        this.times = new byte[24];
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList(
            "dev", "ino", "mode", "nlink", "uid",
            "gid", "padding", "rdev", "bytes", "times"
        );
    }
}
