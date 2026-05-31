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
 * ON-ERROR (replaces legacy try/catch EO object, issue #5166).
 * @since 0.57
 */
@XmirObject(oname = "on-error")
public final class EOonError extends PhDefault implements Atom {

    /**
     * Main attribute name.
     */
    private static final String MAIN = "main";

    /**
     * Handler attribute name.
     */
    private static final String HANDLER = "handler";

    /**
     * Finally attribute name.
     */
    private static final String FINALLY = "finally";

    /**
     * Ctor.
     */
    public EOonError() {
        super(new Attrs(
            new Attr(EOonError.MAIN, new AtVoid(EOonError.MAIN)),
            new Attr(EOonError.HANDLER, new AtVoid(EOonError.HANDLER)),
            new Attr(EOonError.FINALLY, new AtVoid(EOonError.FINALLY))
        ));
    }

    @Override
    public Phi lambda() {
        byte[] result;
        try {
            result = new Dataized(this.take(EOonError.MAIN)).take();
        } catch (final EOerror.ExError ex) {
            final Phi handler = this.take(EOonError.HANDLER).copy();
            handler.put(0, ex.enclosure());
            result = new Dataized(handler).take();
        } finally {
            new Dataized(this.take(EOonError.FINALLY)).take();
        }
        return new Data.ToPhi(result);
    }
}
