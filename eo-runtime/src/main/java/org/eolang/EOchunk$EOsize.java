/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Chunk.size object.
 *
 * @since 0.41.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "chunk.size")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOchunk$EOsize extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOchunk$EOsize() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Heaps.INSTANCE.size(
                new Natural(Expect.at(this.take(Phi.RHO), "id")).it()
            )
        );
    }
}
