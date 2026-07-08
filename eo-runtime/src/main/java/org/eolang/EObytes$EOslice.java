/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import java.util.Arrays;

/**
 * BYTES.SLICE.
 * @since 0.1.0
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
            new Attr("start", new AtVoid("start")),
            new Attr("len", new AtVoid("len")),
            new Attr(EObytes$EOslice.FALLBACK, new AtVoid(EObytes$EOslice.FALLBACK))
        ));
    }

    @Override
    public Phi lambda() {
        final byte[] bytes = new Dataized(this.take(Phi.RHO)).take();
        final int start = this.natural("start");
        final int len = this.natural("len");
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

    /**
     * Read the given attribute as a non-negative integer, aborting when it is
     * not one (a contract violation, not a recoverable failure).
     * @param attr Attribute name
     * @return The attribute as a non-negative int
     */
    private int natural(final String attr) {
        return Expect.at(this, attr)
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .that(Double::intValue)
            .otherwise("must be an integer")
            .must(integer -> integer >= 0)
            .otherwise("must be a positive integer")
            .it();
    }
}
