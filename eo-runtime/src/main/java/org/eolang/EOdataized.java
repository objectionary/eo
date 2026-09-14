/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * DATAIZED.
 *
 * @since 0.74.0
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
