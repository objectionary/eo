/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

import EOorg.EOeolang.EOsys.Syscall;
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
        params[0].put(0, new Data.ToPhi(time.year));
        params[0].put(1, new Data.ToPhi(time.month));
        params[0].put(2, new Data.ToPhi(time.day));
        params[0].put(3, new Data.ToPhi(time.dayOfWeek));
        params[0].put(4, new Data.ToPhi(time.hour));
        params[0].put(5, new Data.ToPhi(time.minute));
        params[0].put(6, new Data.ToPhi(time.second));
        params[0].put(7, new Data.ToPhi(time.milliseconds));
        result.put(1, params[0]);
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
