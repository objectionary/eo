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
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _ftime32_s function call.
 * @since 0.74.0
 */
public final class FtimeFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public FtimeFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final FtimeFuncCall.Timeb timeb = new FtimeFuncCall.Timeb();
        result.put(0, new Data.ToPhi(Msvcrt.INSTANCE._ftime32_s(timeb)));
        final Phi struct = this.win.take("timeb");
        struct.put(0, new Data.ToPhi(timeb.time));
        struct.put(1, new Data.ToPhi(timeb.millitm));
        result.put(1, struct);
        return result;
    }

    /**
     * The {@code struct __timeb32} filled by {@code _ftime32_s}.
     * @since 0.74.0
     * @checkstyle VisibilityModifierCheck (100 lines)
     */
    public static final class Timeb extends Structure {

        /**
         * Seconds since the Unix epoch.
         */
        public int time;

        /**
         * Fraction of a second, in milliseconds.
         */
        public short millitm;

        /**
         * Difference in minutes between UTC and local time.
         */
        public short timezone;

        /**
         * Nonzero when daylight saving time is in effect.
         */
        public short dstflag;

        @Override
        public List<String> getFieldOrder() {
            return Arrays.asList("time", "millitm", "timezone", "dstflag");
        }
    }
}
