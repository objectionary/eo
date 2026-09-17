/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The 64-bit-inode {@code struct stat} of macOS on arm64.
 *
 * @since 0.74.0
 * @checkstyle VisibilityModifierCheck (60 lines)
 */
public final class MacFileStat extends Structure implements StatSyscall.FileStat {

    /**
     * Device id.
     */
    public int dev;

    /**
     * Mode bits.
     */
    public short mode;

    /**
     * Hard link count.
     */
    public short nlink;

    /**
     * Inode number.
     */
    public long ino;

    /**
     * Owner id.
     */
    public int uid;

    /**
     * Group id.
     */
    public int gid;

    /**
     * Device id for special files.
     */
    public int rdev;

    /**
     * Padding before the following timestamps.
     */
    public int padding;

    /**
     * Access, modification, change and birth timestamps EO does not read.
     */
    public byte[] times;

    /**
     * Size in bytes.
     */
    public long bytes;

    /**
     * Block info and flags that EO does not read.
     */
    public byte[] rest;

    /**
     * Ctor.
     */
    public MacFileStat() {
        super();
        this.times = new byte[64];
        this.rest = new byte[40];
    }

    @Override
    public long mode() {
        return this.mode & 0xFFFF;
    }

    @Override
    public long length() {
        return this.bytes;
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList(
            "dev", "mode", "nlink", "ino", "uid",
            "gid", "rdev", "padding", "times", "bytes", "rest"
        );
    }
}
