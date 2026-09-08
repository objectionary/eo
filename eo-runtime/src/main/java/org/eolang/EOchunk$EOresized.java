/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Chunk.resized object.
 *
 * @since 0.41.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "chunk.resized")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOchunk$EOresized extends PhDefault implements Atom {

    /**
     * Name of the void that holds the size the block is resized to.
     */
    private static final String CAPACITY = "capacity";

    /**
     * Ctor.
     */
    public EOchunk$EOresized() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr(
                    EOchunk$EOresized.CAPACITY,
                    new AtVoid(EOchunk$EOresized.CAPACITY)
                )
            )
        );
    }

    @Override
    public Phi lambda() {
        final Phi rho = this.take(Phi.RHO);
        Heaps.INSTANCE.resize(
            new Natural(Expect.at(rho, "id")).it(),
            new Natural(Expect.at(this, EOchunk$EOresized.CAPACITY)).it()
        );
        return rho;
    }
}
