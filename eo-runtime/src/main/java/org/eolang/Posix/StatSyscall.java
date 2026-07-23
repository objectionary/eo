/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.Posix; // NOPMD

import com.sun.jna.Platform;
import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Stat syscall.
 *
 * <p>Fills a {@code struct stat} for the file at the given path and hands its
 * mode bits and byte size to EO. Linux x86-64, Linux aarch64 and macOS lay that
 * struct out differently, so each keeps its own {@link FileStat}; the divergence
 * is spelled out rather than papered over.</p>
 *
 * @since 0.57.0
 */
public final class StatSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public StatSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final String path = new Dataized(params[0]).asString();
        final FileStat info;
        final int code;
        if (Platform.isMac()) {
            final StatSyscall.Mac mac = new StatSyscall.Mac();
            code = CStdLib.INSTANCE.stat(path, mac);
            info = mac;
        } else if (Platform.isARM()) {
            final StatSyscall.LinuxArm arm = new StatSyscall.LinuxArm();
            code = CStdLib.INSTANCE.stat(path, arm);
            info = arm;
        } else {
            final StatSyscall.Linux linux = new StatSyscall.Linux();
            code = CStdLib.INSTANCE.stat(path, linux);
            info = linux;
        }
        result.put(0, new Data.ToPhi(code));
        final Phi struct = this.posix.take("stat");
        struct.put(0, new Data.ToPhi(info.mode()));
        struct.put(1, new Data.ToPhi(info.length()));
        result.put(1, struct);
        return result;
    }

    /**
     * A file's status, exposing the two fields EO reads no matter how the
     * platform's {@code struct stat} is laid out.
     * @since 0.74.0
     */
    public interface FileStat {

        /**
         * File mode bits, carrying the type and permissions.
         * @return Mode bits, masked to sixteen bits
         */
        long mode();

        /**
         * File size.
         * @return Size in bytes
         */
        long length();
    }

    /**
     * The {@code struct stat} of Linux on x86-64.
     * @since 0.74.0
     * @checkstyle VisibilityModifierCheck (60 lines)
     * @checkstyle MagicNumberCheck (60 lines)
     */
    public static final class Linux extends Structure implements FileStat {

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
         * @checkstyle ConstructorsCodeFreeCheck (5 lines)
         */
        public Linux() {
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
     * @checkstyle MagicNumberCheck (60 lines)
     */
    public static final class LinuxArm extends Structure implements FileStat {

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
         * @checkstyle ConstructorsCodeFreeCheck (5 lines)
         */
        public LinuxArm() {
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

    /**
     * The 64-bit-inode {@code struct stat} of macOS on arm64.
     * @since 0.74.0
     * @checkstyle VisibilityModifierCheck (60 lines)
     * @checkstyle MagicNumberCheck (60 lines)
     */
    public static final class Mac extends Structure implements FileStat {

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
         * @checkstyle ConstructorsCodeFreeCheck (6 lines)
         */
        public Mac() {
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
}
