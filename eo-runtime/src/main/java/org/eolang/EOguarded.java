/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Guarded object.
 * @since 0.74.0
 */
@XmirObject(oname = "guarded")
public final class EOguarded extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOguarded() {
        super(new Attrs(
            new Attr("scope", new AtVoid("scope")),
            new Attr("cleanup", new AtVoid("cleanup"))
        ));
    }

    @Override
    public Phi lambda() {
        try {
            return new Data.ToPhi(new Dataized(this.take("scope")).take());
        } finally {
            new Dataized(this.take("cleanup")).take();
        }
    }
}
