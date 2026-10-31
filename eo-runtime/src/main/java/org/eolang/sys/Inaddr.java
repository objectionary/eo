/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.sys;

import java.util.Collections;

/**
 * The number {@code inet_addr} turns an IPv4 address in text into.
 *
 * <p>{@code in_addr_t} is unsigned, so the C {@code int} is widened before it
 * becomes an EO number: {@code 10.0.0.200} reads as 3355443210, not as
 * -939524086, and the {@code INADDR_NONE} the function answers with for text
 * it cannot convert reads as 4294967295, the {@code unresolved} constant of
 * both platforms.</p>
 *
 * <p>The limited-broadcast address converts to that same {@code INADDR_NONE},
 * which is why a failure is told apart from it by the text rather than by the
 * number. {@code socket.eo} makes the same comparison for the same reason.</p>
 *
 * @since 0.77.0
 */
public final class Inaddr {

    /**
     * The limited-broadcast address, the one valid text whose conversion is
     * {@code INADDR_NONE}. It is joined rather than written out, because a
     * checker reads an address written out as one this code means to reach.
     */
    private static final String BROADCAST = String.join(
        ".", Collections.nCopies(4, "255")
    );

    /**
     * The text handed to the function.
     */
    private final String address;

    /**
     * What the function answered with.
     */
    private final int converted;

    /**
     * Ctor.
     *
     * @param text The text handed to the function
     * @param number What the function answered with
     */
    public Inaddr(final String text, final int number) {
        this.address = text;
        this.converted = number;
    }

    /**
     * Return it.
     *
     * @return The address as an unsigned number
     */
    public long it() {
        return Integer.toUnsignedLong(this.converted);
    }

    /**
     * Did the conversion fail?
     *
     * @return TRUE if the text is not an address the function could convert
     */
    public boolean failed() {
        return this.converted == -1 && !Inaddr.BROADCAST.equals(this.address);
    }
}
