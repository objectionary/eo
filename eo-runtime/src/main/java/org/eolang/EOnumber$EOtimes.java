/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Number.times object.
 *
 * @since 0.39.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.times")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOtimes extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOnumber$EOtimes() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Numeric(Expect.at(this, Phi.RHO)).it()
                * new Numeric(Expect.at(this, "x")).it()
        );
    }
}
