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
 * TRY.
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "try")
public final class EOtry extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOtry() {
        super(new Attrs(
            new Attr("main", new AtVoid("main")),
            new Attr("catch", new AtVoid("catch")),
            new Attr("finally", new AtVoid("finally"))
        ));
    }

    @Override
    public Phi lambda() {
        byte[] result;
        try {
            result = new Dataized(this.take("main")).take();
        } catch (final EOerror.ExError ex) {
            final Phi catcher = this.take("catch").copy();
            catcher.put(0, ex.enclosure());
            result = new Dataized(catcher).take();
        } finally {
            new Dataized(this.take("finally")).take();
        }
        return new Data.ToPhi(result);
    }
}
