/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * BYTES.RIGHT.
 *
 * @since 0.1.0
 * @checkstyle IllegalIdentifierNameCheck (16 lines)
 * @checkstyle TypeNameCheck (15 lines)
 */
@XmirObject(oname = "bytes.right")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOright extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EObytes$EOright() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr("x", new AtVoid("x"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO))
                .asBytes()
                .shift(new Int(Expect.at(this, "x")).it())
                .take()
        );
    }
}
