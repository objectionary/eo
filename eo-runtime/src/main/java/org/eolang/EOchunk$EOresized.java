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
 * Chunk.resized object.
 * @since 0.41.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "chunk.resized")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOchunk$EOresized extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOchunk$EOresized() {
        super(new Attrs(new Attr("new-size", new AtVoid("new-size"))));
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public Phi lambda() {
        final Phi rho = this.take(Phi.RHO);
        final int id = new Expect.Natural(Expect.at(rho, "id")).it();
        final int size = new Expect.Natural(Expect.at(this, "new-size")).it();
        Heaps.INSTANCE.resize(id, size);
        return rho;
    }
}
