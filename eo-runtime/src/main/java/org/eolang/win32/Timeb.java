/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The {@code struct __timeb64} filled by {@code _ftime64_s}.
 *
 * @since 0.74.0
 * @checkstyle VisibilityModifierCheck (100 lines)
 */
public final class Timeb extends Structure {

    /**
     * Seconds since the Unix epoch.
     */
    public long time;

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

    /**
     * Ctor.
     */
    public Timeb() {
        // nothing
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList("time", "millitm", "timezone", "dstflag");
    }
}
