/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * BYTES.NOT.
 *
 * @since 0.1.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.not")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOnot extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EObytes$EOnot() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }

    @Override

    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO)).asBytes().not().take()
        );
    }
}
