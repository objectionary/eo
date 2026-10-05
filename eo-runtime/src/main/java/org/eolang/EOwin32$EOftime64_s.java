/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.win32.Msvcrt;
import org.eolang.sys.win32.Timeb;

/**
 * Says what time it is, as the C runtime's `_ftime64_s` does.
 *
 * <p>The call counts in milliseconds where the POSIX one counts in
 * microseconds, so the milliseconds are widened here. Both halves then answer
 * with the same three numbers and a reader of the clock needs to know only
 * one of them.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.ftime64-s")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOftime64_s extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOftime64_s() {
        super(new Attrs());
    }

    @Override
    public Phi lambda() {
        final Timeb timeb = new Timeb();
        final Phi result = Phi.Φ.take("win32").take("time-return").copy();
        result.put(0, new Data.ToPhi(Msvcrt.INSTANCE._ftime64_s(timeb)));
        result.put(1, new Data.ToPhi(timeb.time));
        result.put(2, new Data.ToPhi((long) timeb.millitm * 1000L));
        return result;
    }
}
