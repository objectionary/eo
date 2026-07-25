/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.Win32; // NOPMD

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _stat64 function call.
 *
 * <p>Fills a {@code struct _stat64} and hands its mode bits and byte size to
 * EO. That struct carries a 64-bit {@code st_size} and 64-bit
 * {@code __time64_t} timestamps, so it reports sizes past two gigabytes.</p>
 *
 * @since 0.57.0
 */
public final class Stat64FuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public Stat64FuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final Stat64FuncCall.WinStat info = new Stat64FuncCall.WinStat();
        result.put(
            0,
            new Data.ToPhi(
                Msvcrt.INSTANCE._stat64(new Dataized(params[0]).asString(), info)
            )
        );
        final Phi struct = this.win.take("stat64");
        struct.put(0, new Data.ToPhi((long) (info.mode & 0xFFFF)));
        struct.put(1, new Data.ToPhi(info.bytes));
        result.put(1, struct);
        return result;
    }

    /**
     * The {@code struct _stat64} of the Microsoft C runtime.
     * @since 0.74.0
     * @checkstyle VisibilityModifierCheck (60 lines)
     * @checkstyle MagicNumberCheck (60 lines)
     */
    public static final class WinStat extends Structure {

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
         * @checkstyle ConstructorsCodeFreeCheck (5 lines)
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
}
