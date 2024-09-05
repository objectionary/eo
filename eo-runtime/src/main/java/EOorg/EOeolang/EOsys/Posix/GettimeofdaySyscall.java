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
package EOorg.EOeolang.EOsys.Posix; // NOPMD

import com.sun.jna.Structure;
import EOorg.EOeolang.EOsys.Syscall;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Phi;

/**
 * Gettimeofday syscall.
 * @since 0.40
 */
public final class GettimeofdaySyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public GettimeofdaySyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final GettimeofdaySyscall.Timeval timeval = new GettimeofdaySyscall.Timeval();
        result.put(0, new Data.ToPhi(CStdLib.INSTANCE.gettimeofday(timeval, null)));
        final Phi struct = this.posix.take("timeval").copy();
        struct.put("tv-sec", new Data.ToPhi(timeval.sec));
        struct.put("tv-usec", new Data.ToPhi(timeval.usec));
        result.put(1, struct);
        return result;
    }

    /**
     * Timeval structure.
     * @since 0.40.0
     */
    public static class Timeval extends Structure {
        /**
         * Seconds since Jan. 1, 1970
         */
        public long sec;

        /**
         * Microseconds since Jan. 1, 1970
         */
        public long usec;

        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList("sec", "usec");
        }
    }
}
