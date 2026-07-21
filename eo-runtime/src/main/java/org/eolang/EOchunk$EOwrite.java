/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

/**
 * Chunk.write object.
 * @since 0.36.0
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
            new Attr("offset", new AtVoid("offset")),
            new Attr("data", new AtVoid("data"))
        ));
    }

    @Override
    public Phi lambda() {
        Heaps.INSTANCE.write(
            new Expect.Natural(Expect.at(this.take(Phi.RHO), "id")).it(),
            new Expect.Natural(Expect.at(this, "offset")).it(),
            new Dataized(this.take("data")).take()
        );
        return new Data.ToPhi(true);
    }
}
