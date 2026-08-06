/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * System call that can be made with EO objects ({@link Phi}) as arguments.
 * @since 0.40
 */
@FunctionalInterface
public interface Syscall {

    // @todo #3489:30min Every implementation of this interface wraps its return code in a bare
    //  new PhDefault(), which gets a Silent statistics and therefore never reaches the counters
    //  the program reports. Hand each syscall the statistics of the atom that calls it.

    /**
     * Makes native method call.
     * @param params Native methods parameters
     * @return Methods return code
     */
    Phi make(Phi... params);
}
