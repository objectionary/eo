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
 * Chunk.size object.
 * @since 0.41.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "chunk.size")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOchunk$EOsize extends PhDefault implements Atom {

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Heaps.INSTANCE.size(
                new Expect.Natural(Expect.at(this.take(Phi.RHO), "id")).it()
            )
        );
    }
}
