/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;

/**
 * BYTES.EQ.
 *
 * @since 0.1.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "bytes.eq")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOeq extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EObytes$EOeq() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho()), new Attr("b", new AtVoid("b"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Arrays.equals(
                new Dataized(
                    this.take("b").take("as-bytes")
                ).take(),
                new Dataized(this.take(Phi.RHO)).take()
            )
        );
    }
}
