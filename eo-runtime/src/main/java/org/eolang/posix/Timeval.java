/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * Timeval structure.
 * @since 0.40.0
 * @checkstyle VisibilityModifierCheck (30 lines)
 */
public final class Timeval extends Structure {

    /**
     * Seconds since Jan. 1, 1970
     */
    public long sec;

    /**
     * Microseconds since Jan. 1, 1970
     */
    public long usec;

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
