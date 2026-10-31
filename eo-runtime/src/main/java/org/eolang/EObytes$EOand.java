/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * BYTES.AND.
 *
 * @since 0.1.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.and")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOand extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EObytes$EOand() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr("b", new AtVoid("b"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO)).asBytes().and(
                new Dataized(this.take("b")).asBytes()
            ).take()
        );
    }
}
