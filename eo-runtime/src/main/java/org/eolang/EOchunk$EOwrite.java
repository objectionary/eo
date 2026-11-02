/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Chunk.write object.
 *
 * @since 0.36.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "chunk.write")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOchunk$EOwrite extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOchunk$EOwrite() {
        super(new Attrs(
            new Attr(Phi.RHO, new AtRho()),
            new Attr("v🌵offset", new AtVoid("v🌵offset")),
            new Attr("v🌵data", new AtVoid("v🌵data"))
        ));
    }

    @Override
    public Phi lambda() {
        final int id = new Natural(Expect.at(this.take(Phi.RHO), "id")).it();
        final int offset = new Natural(Expect.at(this, "v🌵offset")).it();
        final byte[] data = new Dataized(this.take("v🌵data")).take();
        Heaps.INSTANCE.write(id, offset, data);
        return new Data.ToPhi(data);
    }
}
