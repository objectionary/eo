/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Number.div object.
 * @since 0.39.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "number.div")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOnumber$EOdiv extends PhDefault implements Atom {

    /**
     * Ctor.
     * @param stats Where this object reports its birth
     */
    public EOnumber$EOdiv(final Statistics stats) {
        super(stats, new Attrs(new Attr("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Expect.Numeric(Expect.at(this, Phi.RHO)).it()
                / new Expect.Numeric(Expect.at(this, "x")).it()
        );
    }
}
