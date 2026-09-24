/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.win32.Msvcrt;

/**
 * Reports the code the last failed msvcrt call left behind, as `_errno` holds it.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.errno")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOerrno extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOerrno() {
        super(new Attrs());
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(Msvcrt.INSTANCE._errno().getInt(0));
    }
}
