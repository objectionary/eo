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
 * Number.times object.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.times")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOtimes extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOnumber$EOtimes() {
        super(new Attrs(new Attr("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Expect.Number(Expect.at(this, Phi.RHO)).it()
                * new Expect.Number(Expect.at(this, "x")).it()
        );
    }
}
