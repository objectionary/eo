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
 * DATAIZED.
 * @since 0.74.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "dataized")
public final class EOdataized extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOdataized() {
        super(new Attrs(new Attr("target", new AtVoid("target"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(new Dataized(this.take("target")).take());
    }
}
