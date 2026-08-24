/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.NativeLong;
import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * Timeval structure.
 *
 * <p>{@code tv_sec} and {@code tv_usec} are C {@code long} fields, 32 bits
 * wide on an ordinary 32-bit POSIX target and 64 bits wide on a 64-bit one.
 * {@link NativeLong} follows that native width itself, instead of always
 * laying the struct out as two 8-byte fields the way a Java {@code long}
 * would (#7574).</p>
 *
 * @since 0.40.0
 * @checkstyle VisibilityModifierCheck (30 lines)
 */
public final class Timeval extends Structure {

    /**
     * Seconds since Jan. 1, 1970
     */
    public NativeLong sec;

    /**
     * Microseconds since Jan. 1, 1970
     */
    public NativeLong usec;

    /**
     * Ctor.
     */
    public Timeval() {
        // nothing
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList("sec", "usec");
    }
}
