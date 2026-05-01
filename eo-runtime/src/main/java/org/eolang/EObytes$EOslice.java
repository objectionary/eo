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
     * Ctor.
     */
    public EObytes$EOslice() {
        super(new Attrs(
            new AttrEntry("start", new AtVoid("start")),
            new AttrEntry("len", new AtVoid("len"))
        ));
    }

    @Override
    public Phi lambda() {
        final byte[] bytes = new Dataized(this.take(Phi.RHO)).take();
        final int start = Expect.at(this, "start")
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .that(Double::intValue)
            .otherwise("must be an integer")
            .must(integer -> integer >= 0)
            .otherwise("must be a positive integer")
            .it();
        return new Data.ToPhi(
            Arrays.copyOfRange(
                bytes,
                start,
                start + Expect.at(this, "len")
                    .that(phi -> new Dataized(phi).asNumber())
                    .otherwise("must be a number")
                    .must(number -> number % 1 == 0)
                    .that(Double::intValue)
                    .otherwise("must be an integer")
                    .must(integer -> integer >= 0)
                    .otherwise("must be a positive integer")
                    .must(integer -> start + integer <= bytes.length).otherwise(
                        String.format("is out of bounds for bytes of size %d", bytes.length)
                    )
                    .it()
            )
        );
    }
}
