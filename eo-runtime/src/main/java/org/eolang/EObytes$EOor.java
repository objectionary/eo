/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * BYTES.OR.
 *
 * @since 0.1.0
 * @checkstyle IllegalIdentifierNameCheck (16 lines)
 * @checkstyle TypeNameCheck (15 lines)
 */
@XmirObject(oname = "bytes.or")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOor extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EObytes$EOor() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr("b", new AtVoid("b"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO)).asBytes().or(
                new Dataized(this.take("b")).asBytes()
            ).take()
        );
    }
}
