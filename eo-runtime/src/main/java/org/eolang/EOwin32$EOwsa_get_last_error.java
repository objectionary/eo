/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Native;
import org.eolang.sys.win32.Winsock;

/**
 * Says what the last Winsock call went wrong with, as `WSAGetLastError` does.
 *
 * <p>The function itself is never called: the slot it reads is overwritten by
 * the machinery standing between a mapped call and the Java code around it,
 * so what is read instead is the copy JNA takes of that slot the instant
 * every mapped call returns. {@link Winsock#WSASetLastError(int)} says more
 * about why.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.wsa-get-last-error")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOwsa_get_last_error extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOwsa_get_last_error() {
        super(new Attrs());
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(Native.getLastError());
    }
}
