/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The {@code struct stat} of Linux on aarch64.
 *
 * <p>The aarch64 kernel reorders the head of the struct relative to x86-64:
 * {@code st_nlink} shrinks to a 32-bit field placed after {@code st_mode},
 * and the padding sits before {@code st_size} rather than before
 * {@code st_rdev}, so the whole thing is 128 bytes instead of 144.</p>
 *
 * @since 0.74.0
 * @checkstyle VisibilityModifierCheck (60 lines)
 */
public final class LinuxArmFileStat extends Structure implements StatSyscall.FileStat {

    /**
     * Device id.
     */
    public long dev;

    /**
     * Inode number.
     */
    public long ino;

    /**
     * Mode bits.
     */
    public int mode;

    /**
     * Hard link count.
     */
    public int nlink;

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
    public long rdev;

    /**
     * Padding before the following size.
     */
    public long padding;

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
    public LinuxArmFileStat() {
        super();
        this.rest = new byte[72];
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
            "dev", "ino", "mode", "nlink", "uid",
            "gid", "rdev", "padding", "bytes", "rest"
        );
    }
}
