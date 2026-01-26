/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Posix; // NOPMD

import EOorg.EOeolang.EOsm.Syscall;
import com.sun.jna.Structure;
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
        final Phi struct = this.posix.take("timeval");
        struct.put(0, new Data.ToPhi(timeval.sec));
        struct.put(1, new Data.ToPhi(timeval.usec));
        result.put(1, struct);
        return result;
    }

    /**
     * Timeval structure.
     * @since 0.40.0
     * @checkstyle VisibilityModifierCheck (30 lines)
     */
    public static final class Timeval extends Structure {
        /**
         * Seconds since Jan. 1, 1970
         */
        public long sec;

        /**
         * Microseconds since Jan. 1, 1970
         */
        public long usec;

        @Override
        public List<String> getFieldOrder() {
            return Arrays.asList("sec", "usec");
        }
    }
}
