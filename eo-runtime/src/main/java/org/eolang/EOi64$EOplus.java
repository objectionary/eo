/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

/**
 * The i64.plus.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "i64.plus")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOi64$EOplus extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOi64$EOplus() {
        super(new Attrs(new Attr("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        final Phi num = Phi.Φ.take("i64").copy();
        num.put(
            0,
            new Data.ToPhi(
                new BytesOf(
                    Long.sum(
                        new Expect.I64(Expect.at(this, Phi.RHO)).it(),
                        new Expect.I64(Expect.at(this, "x")).it()
                    )
                ).take()
            )
        );
        return num;
    }
}
