/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * BYTES.LEFT.
 *
 * <p>A left shift by {@code b} bits is a right shift by the negated
 * count. The smallest count has no negation that fits into an
 * {@code int}, and the negation wraps back onto it, which still shifts
 * left by as many bits as any chain has, so nothing of the chain
 * survives, as it should not.</p>
 *
 * @since 0.78.0
 * @checkstyle IllegalIdentifierNameCheck (21 lines)
 * @checkstyle TypeNameCheck (20 lines)
 */
@XmirObject(oname = "bytes.left")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOleft extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EObytes$EOleft() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr("b", new AtVoid("b"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Dataized(this.take(Phi.RHO))
                .asBytes()
                .shift(-new Int(Expect.at(this, "b")).it())
                .take()
        );
    }
}
