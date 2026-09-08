/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The {@code struct stat} of Linux on x86-64.
 *
 * @since 0.74.0
 * @checkstyle VisibilityModifierCheck (60 lines)
 */
public final class LinuxFileStat extends Structure implements StatSyscall.FileStat {

    /**
     * Device id.
     */
    public long dev;

    /**
     * Inode number.
     */
    public long ino;

    /**
     * Hard link count.
     */
    public long nlink;

    /**
     * Mode bits.
     */
    public int mode;

    /**
     * Owner id.
     */
    public int uid;

    /**
     * Group id.
     */
    public int gid;

    /**
     * Padding before the following device id.
     */
    public int padding;

    /**
     * Device id for special files.
     */
    public long rdev;

    /**
     * Size in bytes.
     */
    public long bytes;

    /**
     * Block info and timestamps that EO does not read.
     */
    public byte[] rest;

    /**
     * Ctor.
     */
    public LinuxFileStat() {
        super();
        this.rest = new byte[88];
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
            "dev", "ino", "nlink", "mode", "uid",
            "gid", "padding", "rdev", "bytes", "rest"
        );
    }
}
