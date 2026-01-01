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
import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Phi;

/**
 * GetSystemTime kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getsystemtime">here for details</a>
 * @since 0.40.0
 */
public final class GetSystemTimeFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public GetSystemTimeFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final GetSystemTimeFuncCall.SystemTime time = new GetSystemTimeFuncCall.SystemTime();
        Kernel32.INSTANCE.GetSystemTime(time);
        result.put(0, new Data.ToPhi(true));
        final Phi struct = this.win.take("system-time");
        struct.put(0, new Data.ToPhi(time.year));
        struct.put(1, new Data.ToPhi(time.month));
        struct.put(2, new Data.ToPhi(time.day));
        struct.put(3, new Data.ToPhi(time.dayOfWeek));
        struct.put(4, new Data.ToPhi(time.hour));
        struct.put(5, new Data.ToPhi(time.minute));
        struct.put(6, new Data.ToPhi(time.second));
        struct.put(7, new Data.ToPhi(time.milliseconds));
        result.put(1, struct);
        return result;
    }

    /**
     * System time structure.
     * @since 0.40.0
     * @checkstyle VisibilityModifierCheck (100 lines)
     */
    public static final class SystemTime extends Structure {
        /**
         * Year.
         */
        public short year;

        /**
         * Month.
         */
        public short month;

        /**
         * Day of week.
         * @checkstyle MemberNameCheck (5 lines)
         */
        public short dayOfWeek;

        /**
         * Day.
         */
        public short day;

        /**
         * Hour.
         */
        public short hour;

        /**
         * Minute.
         */
        public short minute;

        /**
         * Second.
         */
        public short second;

        /**
         * Milliseconds.
         */
        public short milliseconds;

        @Override
        public List<String> getFieldOrder() {
            return Arrays.asList(
                "year",
                "month",
                "day",
                "dayOfWeek",
                "hour",
                "minute",
                "second",
                "milliseconds"
            );
        }
    }
}
