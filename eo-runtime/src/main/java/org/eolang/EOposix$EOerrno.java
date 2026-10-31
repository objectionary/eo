/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Native;

/**
 * Reports the code the last failed call left behind, as `errno` holds it.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.errno")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOerrno extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOerrno() {
        super(new Attrs());
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(Native.getLastError());
    }
}
