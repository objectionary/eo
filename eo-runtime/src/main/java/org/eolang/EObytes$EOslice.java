/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;

/**
 * BYTES.SLICE.
 *
 * @since 0.1.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.slice")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOslice extends PhDefault implements Atom {

    /**
     * Name of the error-branch void that holds the caller's slice fallback.
     */
    private static final String FALLBACK = "cant-slice";

    /**
     * Ctor.
     */
    public EObytes$EOslice() {
        super(new Attrs(
            new Attr(Phi.RHO, new AtRho()),
            new Attr("start", new AtVoid("start")),
            new Attr("len", new AtVoid("len")),
            new Attr(EObytes$EOslice.FALLBACK, new AtVoid(EObytes$EOslice.FALLBACK))
        ));
    }

    @Override
    public Phi lambda() {
        final byte[] bytes = new Dataized(this.take(Phi.RHO)).take();
        final int start = new Natural(Expect.at(this, "start")).it();
        final int len = new Natural(Expect.at(this, "len")).it();
        final Phi result;
        if ((long) start + len <= bytes.length) {
            result = new Data.ToPhi(Arrays.copyOfRange(bytes, start, start + len));
        } else {
            result = this.take(EObytes$EOslice.FALLBACK);
            result.put(
                0,
                new Data.ToPhi(
                    String.format(
                        "cannot slice '%d' bytes from offset '%d' of bytes of size %d",
                        len, start, bytes.length
                    )
                )
            );
        }
        return result;
    }
}
