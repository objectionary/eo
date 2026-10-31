/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Timeval;

/**
 * Says what time it is, as POSIX `gettimeofday` does.
 *
 * <p>The time zone the call also takes is never asked for: POSIX leaves what
 * it means unspecified and every library it was ever meant for is gone.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.gettimeofday")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOgettimeofday extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOgettimeofday() {
        super(new Attrs());
    }

    @Override
    public Phi lambda() {
        final Timeval timeval = new Timeval();
        final Phi result = Phi.Φ.take("posix").take("time-return").copy();
        result.put(0, new Data.ToPhi(CStdLib.INSTANCE.gettimeofday(timeval, null)));
        result.put(1, new Data.ToPhi(timeval.sec.longValue()));
        result.put(2, new Data.ToPhi(timeval.usec.longValue()));
        return result;
    }
}
