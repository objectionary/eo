/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOsm.Win32; // NOPMD

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.EOsm.Syscall;
import org.eolang.Phi;

/**
 * The msvcrt _stat function call.
 *
 * <p>Fills a {@code struct _stat} and hands its mode bits and byte size to EO.
 * That struct keeps a 32-bit {@code st_size}, so sizes are reported only up to
 * two gigabytes; its timestamp tail is over-allocated so a 32- or 64-bit
 * {@code time_t} cannot overrun the buffer.</p>
 *
 * @since 0.57.0
 */
public final class StatFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public StatFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final StatFuncCall.WinStat info = new StatFuncCall.WinStat();
        result.put(
            0,
            new Data.ToPhi(
                Msvcrt.INSTANCE.stat(new Dataized(params[0]).asString(), info)
            )
        );
        final Phi struct = this.win.take("stat");
        struct.put(0, new Data.ToPhi((long) (info.mode & 0xFFFF)));
        struct.put(1, new Data.ToPhi((long) info.bytes));
        result.put(1, struct);
        return result;
    }

    /**
     * The {@code struct _stat} of the Microsoft C runtime.
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
        public int bytes;

        /**
         * Access, modification and change timestamps EO does not read.
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
